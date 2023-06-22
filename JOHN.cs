using System.Collections;
using System.Globalization;
using System.Reflection;
using System.Text.RegularExpressions;

namespace JOHNCS;

public static partial class JOHN
{
    internal struct Token
    {
        public string TokenString;
        public TokenTypes TokenType;
        public int LineNumber;
        public int ColumnNumber;
        public Token(string TokenString, TokenTypes TokenType, int LineNumber, int ColumnNumber)
        {
            this.TokenType = TokenType;
            this.TokenString = TokenString;
            this.LineNumber = LineNumber;
            this.ColumnNumber = ColumnNumber;
        }
    }
    internal static CultureInfo ci = new("en-US");
    internal enum TokenTypes
    {
        Identifier, Value,
        LeftCurlyBrace, RightCurlyBrace,
        LeftSquareBrace, RightSquareBrace,
        LeftParanthesis, RightParanthesis,
        Link
    }

    internal static Dictionary<string, TokenTypes> TokenDict = new() {
        { "{", TokenTypes.LeftCurlyBrace },
        { "}", TokenTypes.RightCurlyBrace },
        { "[", TokenTypes.LeftSquareBrace },
        { "]", TokenTypes.RightSquareBrace },
        { "(", TokenTypes.LeftParanthesis },
        { ")", TokenTypes.RightParanthesis },
    };


    public static T? Parse<T>(string input)
    {
        Token[] tokens = Tokenize(input)
            .Select((t) => new Token(t.Item1, TokenType(t.Item1), t.Item2, t.Item3 - t.Item1.Length)).ToArray();
        return ParseTokens<T>(tokens);
    }

    internal static object? Genericize(string method, Type t, params object[] args)
    {
        MethodInfo? a = typeof(JOHN).GetMethod(method, BindingFlags.NonPublic | BindingFlags.Static);
        return a?.MakeGenericMethod(t).Invoke(null, args);
    }
    internal static T? ParseTokens<T>(Token[] tokens)
    {
        T returnedObject = Activator.CreateInstance<T>();
        if (tokens is null || tokens.Length == 0)
        {
            return returnedObject;
        }

        // Single value parsing
        if (tokens[0].TokenType == TokenTypes.Value && tokens.Length == 1)
            return ParseValue<T>(tokens[0].TokenString);
        // First token must be an identifier otherwise
        if (tokens[0].TokenType != TokenTypes.Identifier)
        {
            throw new JOHNError(
                $"Parser Error: Line {tokens[0].LineNumber}, Column {tokens[0].ColumnNumber}: Expected an identifier, instead found {tokens[0].TokenType}."
            );
        }

        int i = 0;
        int closingIndex;
        PropertyInfo? pi;
        while (i < tokens.Length)
        {
            string identifier = tokens[i++].TokenString;
            pi = typeof(T).GetProperty(identifier);
            switch (tokens[i].TokenType)
            {
                case TokenTypes.Identifier:
                    throw new JOHNError(
                        $"Parser error: Line {tokens[i].LineNumber}, Column {tokens[i].ColumnNumber}: Invalid object content: \"{tokens[i].TokenString}\""
                    );
                case TokenTypes.RightSquareBrace:
                case TokenTypes.RightParanthesis:
                case TokenTypes.RightCurlyBrace:
                    // This shouldn't happen tbh
                    throw new JOHNError(
                        $"Parser error: Line {tokens[i].LineNumber}, Column {tokens[i].ColumnNumber}: Unopened \"{tokens[i].TokenString}\" found"
                    );
                case TokenTypes.Link:
                    if (string.IsNullOrEmpty(identifier))
                    {
                        throw new JOHNError($"Parser error: Line {tokens[i].LineNumber}, Column {tokens[i].ColumnNumber}: Unexpected Link Symbol");
                    }
                    i++;
                    Type nodeType = pi?.PropertyType.GenericTypeArguments.Length > 0 ? pi.PropertyType.GenericTypeArguments[0] : typeof(object);
                    ArrayList llist = new();
                    while (i < tokens.Length
                        && tokens[i].TokenType != TokenTypes.Identifier
                        && tokens[i].TokenType != TokenTypes.Link)
                    {
                        switch (tokens[i].TokenType)
                        {
                            case TokenTypes.Value:
                                llist.Add(Genericize("ParseValue", nodeType, new[] { tokens[i++].TokenString }));
                                break;
                            case TokenTypes.LeftCurlyBrace:
                                closingIndex = FindMatchingBracket(tokens, i);
                                llist.Add(Genericize("ParseTokens", nodeType, new[] { tokens[(i + 1)..closingIndex] }));
                                i = closingIndex + 1;
                                break;
                            case TokenTypes.LeftSquareBrace:
                                closingIndex = FindMatchingBracket(tokens, i);
                                llist.Add(Genericize("ParseArray", nodeType, new[] { tokens[(i + 1)..closingIndex] }));
                                i = closingIndex + 1;
                                break;
                            case TokenTypes.LeftParanthesis:
                                closingIndex = FindMatchingBracket(tokens, i);
                                llist.Add(Genericize("ParseTuple", nodeType, new[] { tokens[(i + 1)..closingIndex] }));
                                i = closingIndex + 1;
                                break;
                            default:
                                throw new JOHNError(
                                    $"Parser Error: Line {tokens[i].LineNumber}, Column {tokens[i].ColumnNumber}: Unmatched \"{tokens[i].TokenString}\" found in Linked List."
                                );
                        }
                    }
                    Type jn = typeof(JOHNNode<>).MakeGenericType(nodeType);
                    dynamic? nodes = Activator.CreateInstance(typeof(List<>).MakeGenericType(jn)) ?? throw new JOHNError($"Invalid JOHNNode<T> Type.");
                    nodes.Add((dynamic?)Activator.CreateInstance(jn, new[] { llist[0], null }));
                    for (int j = 1; j < llist.Count; j++)
                    {
                        nodes.Add((dynamic?)Activator.CreateInstance(jn, new[] { llist[j], null }));
                        nodes[j - 1].Next = nodes[j];
                    }
                    if (i < tokens.Length && tokens[i].TokenType != TokenTypes.Identifier)
                    {
                        nodes[llist.Count - 1].Next = nodes[0];
                        i++;
                    }
                    pi?.SetValue(returnedObject, nodes[0]);
                    break;
                case TokenTypes.Value:
                    if (pi is null) i++;
                    else pi.SetValue(returnedObject, Genericize("ParseValue", pi.PropertyType, new[] { tokens[i++].TokenString }));
                    break;
                case TokenTypes.LeftCurlyBrace:
                    closingIndex = FindMatchingBracket(tokens, i);
                    pi?.SetValue(returnedObject, Genericize("ParseTokens", pi.PropertyType, new[] { tokens[(i + 1)..closingIndex] }));
                    i = closingIndex + 1;
                    break;
                case TokenTypes.LeftSquareBrace:
                    closingIndex = FindMatchingBracket(tokens, i);
                    Type? elt = pi?.PropertyType.GetElementType();
                    if (elt is null)
                    {
                        i = closingIndex + 1; break;
                    }
                    pi?.SetValue(returnedObject, Genericize("ParseArray", elt, new[] { tokens[(i + 1)..closingIndex] }));
                    i = closingIndex + 1;
                    break;
                case TokenTypes.LeftParanthesis:
                    closingIndex = FindMatchingBracket(tokens, i);
                    pi?.SetValue(returnedObject, Genericize("ParseTuple", pi.PropertyType, new[] { tokens[(i + 1)..closingIndex] }));
                    i = closingIndex + 1;
                    break;
            }
        }
        return returnedObject;
    }
    [GeneratedRegex(@"^[\""\'].*[\""\']$")]
    internal static partial Regex StringChar();
    [GeneratedRegex(@"^-?(?:(?:\d+)|(?:\d*\.\d+))([ui](?:8|16|32|64)|f(?:32|64))?$")]
    internal static partial Regex NumberType();
    [GeneratedRegex(@"^v(?:\d+\.){2,3}\d+$")]
    internal static partial Regex VersionR();
    [GeneratedRegex(@"^([\*\^])\d+")]
    internal static partial Regex IndexR();
    [GeneratedRegex(@"^\d+(?:\.\d+)?\.\.\d+(?:\.\d+)?(?:\.\.\d+(?:\.\d+)?)?$")]
    internal static partial Regex RangeR();

    internal static object? ParseValueInternal(string token)
    {
        if (token == "#") return null;
        if (StringChar().IsMatch(token))
        {
            return Regex.Unescape(token)[1..^1];
        }
        if (token is "true" or "false") return token == "true";
        Match dmatches = NumberType().Match(token);
        if (dmatches.Success)
        {
            if (dmatches.Groups[1].Success)
            {
                token = token[0..(token.Length - dmatches.Groups[1].Length)];
                switch (dmatches.Groups[1].Value)
                {
                    case "i8": return Convert.ToSByte(token);
                    case "i16": return Convert.ToInt16(token);
                    case "i32": return Convert.ToInt32(token);
                    case "i64": return Convert.ToInt64(token);
                    case "f32": return Convert.ToSingle(token, ci);
                    case "f64": return Convert.ToDouble(token, ci);
                    case "u8": return Convert.ToByte(token);
                    case "u16": return Convert.ToUInt16(token);
                    case "u32": return Convert.ToUInt32(token);
                    case "u64": return Convert.ToUInt64(token);
                    default: break;
                }
            }
            else
            {
                return token.Contains('.') ? Convert.ToSingle(token, ci) : (object)Convert.ToInt32(token);
            }
        }
        if (token.StartsWith("0"))
        {
            return token[2] == 'b'
                ? Convert.ToInt32(token[2..], 2)
                : token[2] == 'x' ? (object)Convert.ToInt32(token[2..], 16) : throw new JOHNError($"Unknown number base: {token}");
        }

        if (VersionR().IsMatch(token))
        {
            string[] elements = token[1..].Split(".");
            return new Version(
                major: Convert.ToInt32(elements[0]),
                minor: Convert.ToInt32(elements[1]),
                build: Convert.ToInt32(elements.Length > 2 ? elements[2] : 0),
                revision: Convert.ToInt32(elements.Length > 3 ? elements[3] : 0)
            );
        }
        if (IndexR().IsMatch(token))
        {
            return new Index(Convert.ToInt32(token[1..]), token[0] == '^');
        }
        if (RangeR().IsMatch(token))
        {
            double[] elements = token.Split("..").Select(Convert.ToDouble).ToArray();
            if (elements.Length == 2 && elements.All(x => x % 1 == 0))
                return new Range((int)elements[0], (int)elements[1]);
            List<double> r = new();
            for (double i = elements[0]; i < elements[1]; i += elements.Length > 2 ? elements[2] : 1)
            {
                r.Add(i);
            }
            return r.ToArray();
        }

        throw new JOHNError($"Unrecognized Value Type: " + token);
    }
    internal static T? ParseValue<T>(string token)
    {
        try
        {
            return (T?)Convert.ChangeType(ParseValueInternal(token), typeof(T));
        }
        catch
        {
            return default;
        }
    }
    internal static int FindMatchingBracket(Token[] tokens, int index)
    {
        TokenTypes LeftB = tokens[index].TokenType;
        TokenTypes RightB = LeftB == TokenTypes.LeftCurlyBrace
            ? TokenTypes.RightCurlyBrace
            : LeftB == TokenTypes.LeftSquareBrace
            ? TokenTypes.RightSquareBrace
            : TokenTypes.RightParanthesis;
        int nestingLevel = 0;
        for (int i = index; i < tokens.Length; i++)
        {
            if (tokens[i].TokenType == LeftB) nestingLevel++;
            if (tokens[i].TokenType == RightB)
            {
                if (--nestingLevel == 0) return i;
            }
        }
        throw new JOHNError($"Parser Error: Line {tokens[index].LineNumber}, Column {tokens[index].ColumnNumber}: No matching bracket found");
    }
    internal static T? ParseTuple<T>(Token[] tokens)
    {
        Type? tupleType = typeof(T);
        if (tupleType is null) return default;
        if (tupleType.IsGenericType && tupleType.GetGenericTypeDefinition() == typeof(Nullable<>))
        {
            tupleType = tupleType.GenericTypeArguments[0];
        }
        FieldInfo[] fields = tupleType.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
        List<object?> values = new();
        int currentField = 0;
        int closingIndex;
        for (int i = 0; i < tokens.Length; i++)
        {
            if (tokens[i].TokenType == TokenTypes.Identifier)
            {
                throw new JOHNError($"Parser Error: Line {tokens[i].LineNumber}, Column {tokens[i].ColumnNumber}: Tuples cannot contain identifiers");
            }
            switch (tokens[i].TokenType)
            {
                case TokenTypes.RightParanthesis:
                case TokenTypes.RightSquareBrace:
                case TokenTypes.RightCurlyBrace: // <- THIS SHOULDN'T HAPPEN
                    throw new JOHNError("How the hell did you do that");
                case TokenTypes.Link:
                    throw new JOHNError($"Parser Error: Line {tokens[i].LineNumber}, Column {tokens[i].ColumnNumber}: Linked Lists can only be used at Top Level in Objects");
                case TokenTypes.Value:
                    if (fields.Length <= currentField) break;
                    values.Add(Genericize("ParseValue", fields[currentField++].FieldType, new[] { tokens[i].TokenString }));
                    break;
                case TokenTypes.LeftCurlyBrace:
                    closingIndex = FindMatchingBracket(tokens, i);
                    values.Add(Genericize("ParseTokens", fields[currentField++].FieldType, new[] { tokens[(i + 1)..closingIndex] }));
                    i = closingIndex;
                    break;
                case TokenTypes.LeftSquareBrace:
                    closingIndex = FindMatchingBracket(tokens, i);
                    Type? elt = fields[currentField++].FieldType.GetElementType();
                    if (elt is null)
                    {
                        i = closingIndex; break;
                    }
                    values.Add(Genericize("ParseArray", elt, new[] { tokens[(i + 1)..closingIndex] }));
                    i = closingIndex;
                    break;
                case TokenTypes.LeftParanthesis:
                    closingIndex = FindMatchingBracket(tokens, i);
                    values.Add(Genericize("ParseTuple", fields[currentField++].FieldType, new[] { tokens[(i + 1)..closingIndex] }));
                    i = closingIndex;
                    break;
            }
        }
        dynamic? tuple = Activator.CreateInstance(tupleType, values.ToArray());
        return tuple;
    }

    internal static T?[] ParseArray<T>(Token[] tokens)
    {
        List<T?> arr = new();
        int closingIndex;
        for (int i = 0; i < tokens.Length; i++)
        {
            if (tokens[i].TokenType == TokenTypes.Identifier)
            {
                throw new JOHNError($"Parser Error: Line {tokens[i].LineNumber}, Column {tokens[i].ColumnNumber}: Arrays cannot contain identifiers");
            }
            switch (tokens[i].TokenType)
            {
                case TokenTypes.RightParanthesis:
                case TokenTypes.RightSquareBrace:
                case TokenTypes.RightCurlyBrace: // <- THIS SHOULDN'T HAPPEN
                    throw new JOHNError("How the hell did you do that");
                case TokenTypes.Link:
                    throw new JOHNError($"Parser Error: Line {tokens[i].LineNumber}, Column {tokens[i].ColumnNumber}: Linked Lists can only be used at Top Level in Objects");
                case TokenTypes.Value:
                    arr.Add(ParseValue<T>(tokens[i].TokenString) ?? default);
                    break;
                case TokenTypes.LeftCurlyBrace:
                    closingIndex = FindMatchingBracket(tokens, i);
                    arr.Add(ParseTokens<T>(tokens[(i + 1)..closingIndex]));
                    i = closingIndex;
                    break;
                case TokenTypes.LeftSquareBrace:
                    closingIndex = FindMatchingBracket(tokens, i);
                    Type? elt = typeof(T).GetElementType();
                    if (elt is null)
                    {
                        i = closingIndex; break;
                    }
                    arr.Add((T?)Genericize("ParseArray", elt, new[] { tokens[(i + 1)..closingIndex] }));
                    i = closingIndex;
                    break;
                case TokenTypes.LeftParanthesis:
                    closingIndex = FindMatchingBracket(tokens, i);
                    arr.Add(ParseTuple<T>(tokens[(i + 1)..closingIndex]));
                    i = closingIndex;
                    break;
            }
        }
        return arr.ToArray();
    }
    public class JOHNNode<T>
    {
        public T Value { get; set; }
        public JOHNNode<T>? Next { get; set; }
        public JOHNNode(T Value, JOHNNode<T>? Next = null)
        {
            this.Value = Value;
            this.Next = Next;
        }
    }

    public static string Serialize(dynamic obj)
    {
        string johntext = "";
        Type type = obj.GetType();
        if (type.IsArray)
            return "[" + SerializeArray((dynamic[])obj) + "]";
        if (type.IsGenericType && type.GetGenericTypeDefinition().BaseType == typeof(ValueTuple<>).BaseType)
        {
            return "(" + SerializeTuple(obj) + ")";
        }

        else if (type.IsPrimitive || type == typeof(string))
            return StringifyPrimitive(obj);
        else if (obj is null)
            return "#";

        if (obj is Version v)
        {
            return $" v{v.Major}.{v.Minor}.{v.Build}.{v.Revision} ";
        }
        if (obj is Index i)
        {
            return $"{(i.IsFromEnd ? "^" : "*")}{i.Value}";
        }

        if (obj is Range r)
        {
            return $"{r.Start}..{r.End}";
        }
        if (type.IsGenericType && type.GetGenericTypeDefinition() == typeof(JOHNNode<>))
        {
            string str = "-> ";
            dynamic currentNode = obj;
            while (currentNode.Next is not null)
            {
                str += Serialize(currentNode.Value) + " ";
                currentNode = currentNode.Next;
            }
            str += currentNode.Value;
            return str;
        }

        foreach (PropertyInfo key in type.GetProperties())
        {
            johntext += " " + key.Name + " ";
            dynamic o = key.GetValue(obj);
            if (o is null)
            {
                johntext += "# ";
                continue;
            }
            Type t = o.GetType();
            if (!t.IsArray && !t.IsPrimitive && t != typeof(string) && !t.IsGenericType && o is not Version and not Index and not Range)
            {
                johntext += " { " + Serialize(o) + " } ";
            }
            else johntext += Serialize(o);
        }
        return johntext;
    }

    [GeneratedRegex(@"\s*[\[\{\(\}\]\)]\s*")]
    private static partial Regex Minifier();
    [GeneratedRegex("\\s+")]
    private static partial Regex Replacer();
    public static string Minify(string johntext)
    {
        return Minifier().Replace(
            Tokenize(johntext).Select(t => t.Item1).Aggregate(SpaceJoin),
            (s) => Replacer().Replace(s.Value, "")
        );
    }

    static string SpaceJoin(string a, string b)
    {
        return a + " " + b;
    }

    private static (string, int, int)[] Tokenize(string johntext)
    {
        string input = johntext.Trim().Replace("\r", "");
        bool str = false;
        bool chr = false;
        List<(string, int, int)> tokens = new();
        string cTokenstring = "";
        int line = 1;
        int col = 0;

        for (int i = 0; i < input.Length; i++)
        {
            switch (input[i])
            {
                case '"':
                    cTokenstring += input[i];
                    if (!chr)
                    {
                        if (str)
                        {
                            tokens.Add((cTokenstring, line, col));
                            cTokenstring = "";
                        }
                        str = !str;
                    }
                    break;
                case '\'':
                    cTokenstring += input[i];
                    if (!str)
                    {
                        if (chr)
                        {
                            if (cTokenstring.Length > 3)
                            {
                                throw new JOHNError(
                                    $"Tokenizer Error: Line {line}, Column {col}: Char cannot contain more than 1 character."
                                );
                            }
                            tokens.Add((cTokenstring, line, col));
                            cTokenstring = "";
                        }
                        chr = !chr;
                    }
                    break;
                case ':':
                case ';':
                case ',':
                case ' ':
                    if (str || chr) cTokenstring += input[i];
                    else
                    {
                        if (!string.IsNullOrEmpty(cTokenstring)) tokens.Add((cTokenstring, line, col));
                        cTokenstring = "";
                    }
                    break;

                case '\n':
                    if (str || chr)
                    {
                        throw new JOHNError(
                            $"Tokenizer error: Line {line}, Column {col}: Literal may not contain a newline. To include a newline use \\n instead."
                        );
                    }
                    if (!string.IsNullOrEmpty(cTokenstring)) tokens.Add((cTokenstring, line, col));
                    cTokenstring = "";
                    line++;
                    col = -1;
                    break;
                case '\\':
                    cTokenstring += input.Length > i + 1 ? input[i + 1] : input[i];
                    i++;
                    break;
                case '{':
                case '}':
                case '[':
                case ']':
                case '(':
                case ')':
                    if (str || chr) cTokenstring += input[i];
                    else
                    {
                        if (!string.IsNullOrEmpty(cTokenstring))
                            tokens.Add((cTokenstring, line, col));
                        tokens.Add((input[i].ToString(), line, col));
                        cTokenstring = "";
                    }
                    break;
                case '/':
                    if (str || chr || input.Length <= i + 1 || input[i + 1] != '/')
                        cTokenstring += input[i];
                    else
                    {
                        if (!string.IsNullOrEmpty(cTokenstring))
                            tokens.Add((cTokenstring, line, col));
                        cTokenstring = "";
                        i = input.IndexOf('\n', i);
                        if (i < 0) i = input.Length - 1;
                        line++;
                    }
                    break;

                default:
                    cTokenstring += input[i];
                    break;
            }
            col++;
        }
        if (!string.IsNullOrEmpty(cTokenstring)) tokens.Add((cTokenstring, line, col));
        if (str || chr)
        {
            int unmatchedQuoteLine = 1 + input.ToCharArray().Where((c, i) => c == '\n' && i <= input.LastIndexOf(str ? '"' : '\'')).Count();
            throw new JOHNError($"Tokenizer Error: Unmatched quote at line {unmatchedQuoteLine}");
        }
        return tokens.ToArray();
    }

    private static string StringifyPrimitive(dynamic obj)
    {
        Type objtype = obj.GetType();
        if (objtype == typeof(int)) return $"{obj}";
        if (objtype == typeof(long)) return $"{obj}i64";
        if (objtype == typeof(float)) return $"{((float)obj).ToString("F350", ci).TrimEnd('0')}";
        if (objtype == typeof(double)) return $"{((double)obj).ToString("F350", ci).TrimEnd('0')}f64";
        if (objtype == typeof(decimal)) return $"{((decimal)obj).ToString("F350", ci).TrimEnd('0')}f64"; // no decimal support
        if (objtype == typeof(short)) return $"{obj}i16";
        if (objtype == typeof(byte)) return $"{obj}u8";
        if (objtype == typeof(sbyte)) return $"{obj}i8";
        if (objtype == typeof(ushort)) return $"{obj}u16";
        if (objtype == typeof(uint)) return $"{obj}u32";
        if (objtype == typeof(ulong)) return $"{obj}u64";
        if (objtype == typeof(char)) return obj == '\0' ? $"'\\0'" : $"'{Regex.Escape(obj.ToString())}'";
        if (objtype == typeof(bool)) return $"{obj.ToString().ToLower()}";
        return objtype == typeof(string) ? @$"""{Regex.Escape(obj)}""" : obj is null ? "#" : $@"""{obj}""";
    }

    private static string SerializeArray(dynamic[] arr)
    {
        string ser = "";
        for (int i = 0; i < arr.Length; i++)
        {
            Type type = arr[i].GetType();
            if (!type.IsArray && !type.IsGenericType && !type.IsPrimitive && type != typeof(string) && arr[i] is not Version and not Index and not Range)
                ser += " { " + Serialize(arr[i]) + " } ";
            else ser += Serialize(arr[i]);
            ser += " ";
        }
        return ser;
    }

    private static string SerializeTuple(dynamic tuple)
    {
        Type tupleType = tuple.GetType();
        FieldInfo[] itemProperties = tupleType.GetFields();
        dynamic[] itemValues = itemProperties.Select(prop => prop.GetValue(tuple)).ToArray();
        string ser = "";
        for (int i = 0; i < itemValues.Length; i++)
        {
            Type type = itemValues[i].GetType();
            if (!type.IsArray && !type.IsGenericType && !type.IsPrimitive && type != typeof(string) && itemValues[i] is not Version and not Index and not Range)
                ser += " { " + Serialize(itemValues[i]) + " } ";
            else ser += Serialize(itemValues[i]);
            ser += " ";
        }
        return ser;
    }

    private static TokenTypes TokenType(string tokenstring)
    {
        if ("([{}])".ToCharArray().Select(c => c.ToString()).Contains(tokenstring))
            return TokenDict[tokenstring];
        else return tokenstring == "->" ? TokenTypes.Link : ValidateIdentifier(tokenstring) ? TokenTypes.Identifier : TokenTypes.Value;
    }

    [GeneratedRegex(@"^[a-zA-Z_]+[a-zA-Z0-9_-]*$")]
    private static partial Regex IdentifierRegex();

    private static bool ValidateIdentifier(string tokenstring)
    {
        return IdentifierRegex().IsMatch(tokenstring)
               && tokenstring is not ("true" or "false" or "#");
    }
}



public class JOHNError : Exception
{
    public JOHNError(string message) : base(message) { }
}