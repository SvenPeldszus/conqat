using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApplication1
{
    delegate int del(int i);

    delegate void Printer(string s);
    
    class Program
    {
        static void Main(string[] args)
        {
            // === lambdas

            del myDelegate = x => x * x;
            int j = myDelegate(5);
            Expression<del> myET = x => x * x;

            int i2 = apply2((x, y) => { int z = x * y; return z; }, 3, 4);

            for (int i = 0; apply((int x) => x < 5, i); ++i)
            {
                Console.WriteLine(i);
            }

            // === delegates ===

            Printer p = delegate(string s)
            {
                System.Console.WriteLine(s);
            };

            Printer p2 = s => { System.Console.WriteLine(s); };
        }


        static bool apply(Func<int, bool> f, int x)
        {
            return f(x);
        }

        static int apply2(Func<int, int, int> f, int x, int y)
        {
            return f(x, y);
        }
    }
}
