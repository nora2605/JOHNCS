using JOHNCS;
using System.Text.Json;

Person ts = JOHN.Parse<Person>(
@"
Name ""Marvin""
Age 20
"
) ?? new Person();

Console.WriteLine(JsonSerializer.Serialize(ts));
Console.ReadLine();
Console.WriteLine(JOHN.Serialize(ts));
Console.ReadLine();
Console.WriteLine(JOHN.Minify(JOHN.Serialize(ts)));

class Person
{
    public string Name { get; set; } = "";
    public int Age { get; set; }
}
