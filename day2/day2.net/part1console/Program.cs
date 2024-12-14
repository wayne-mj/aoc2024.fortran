// See https://aka.ms/new-console-template for more information
// Console.WriteLine("Hello, World!");

string path = "../input1";
//string path = "../testinput.txt";
int safecount = 0;

string[] str = {};

try{
    str =File.ReadLines(path).ToArray();
}
catch{
    Console.WriteLine($"{path} not found");
}

foreach (var s in str)
{
    int[] num = s.Split(" ").Select(int.Parse).ToArray();
    if (isSafe(num))
    {
        safecount ++;
    }
}

Console.WriteLine(safecount);

bool isSafe(int[] numbers)
{
    bool safe = false;
    bool up = false; bool down = false;

    for (int i=0; i < numbers.Count() -1; i++)
    {
        int dNums = numbers[i] - numbers[i+1];

        if ((dNums == 0) || ( Math.Abs(dNums) > 3))
        {
            safe = false;
            break;
        }

        if (numbers[i] > numbers[i+1])
        {
            up = true;
        }

        if (numbers[i] < numbers[i+1])
        {
            down = true;
        }

        if (up == down)
        {
            safe = false;
            break;
        }
        safe =true;
    }
    

    return safe;
}


