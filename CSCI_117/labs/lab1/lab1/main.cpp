//
//  Lab2 (CONTINUED FROM LAB 1)
//  CSCI 117 - Spring 2019
//
//  Created by Bryce Remick on 9/6/19.
//  Copyright © 2019 Bryce Remick. All rights reserved.
//

#include <iostream>
#include <cstdlib> //for atoi() #include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <cctype>


using namespace std;
int Exp(), Term(), Exp2(int), Term2(int), Fact(), Fact2(int), Num();
string prog; //string for reading 1-line input expression (program) int indexx = 0; //global index for program string
int indexx = 0; //global index for program string


// function to strip whitespaces from input string
void stripWhitespaces()
{
    prog.erase(std::remove(prog.begin(), prog.end(), ' '),
               prog.end());
}

int main(int argc, const char **argv) {
    
    // read file and get line
    ifstream myfile ("data1.txt");
    if (myfile.is_open())
        // get line from file (w/o a while loop it doesnt work correctly?)
        while ( getline (myfile,prog) ){}
    else
        cout << "Unable to open file";
    
   
    cout << prog << endl;
    stripWhitespaces(); // strip spaces
    cout << prog << endl;
    
    //entry point
    int o = Exp();
    
    // print results
    cout << "result="<< o <<endl;
    
}
int Exp()
{
    cout << "Exp" << endl;
    return Exp2(Term());
}
int Term()
{
    cout << "Term" << endl;
    return Term2(Fact());
}

int Exp2(int inp)
{
    
    cout << "Exp2" << endl;
    int result = inp;
    if (indexx < prog.length())
    {
        char a = prog.at(indexx++);
        if (a == '+')
            result = Exp2(result + Term());
        else if (a == '-')
            result = Exp2(result - Term());
        
    }
    return result;
}

int Term2(int inp) {
    cout << "Term2" << endl;
    int result = inp;
    //if not the end of program string
    if (indexx < prog.length())
    {
        char a = prog.at(indexx++); //get one chr from program string
        if (a == '*')
            result = Term2(result * Fact()); //handles consecutive * operators
        else if (a == '/')
            result = Term2(result / Fact());
        else if (a == '+' || a == '-' || a == ')') // backtrack if Exp2
            indexx--;
    }
    return result;
}

int Fact()
{
    cout << "Fact" << endl;
    return Fact2(Num());
}
int Fact2(int inp)
{
    cout << "Fact2" << endl;
    int result = inp;
    
    if (indexx < prog.length())
    {
        char a = prog.at(indexx++); //get one chr from program string
        
        if (a == '^')
            result = Fact2(pow(result, Fact())); // recursive power operator
        else
            indexx--;   // backtrack if not Fact2

    }
    return result;
}

int Num()
{
    cout << "Num" <<  endl;
    char a = prog.at(indexx++); //get one chr from program string

    if (a == '(')   // if beginning of parenthesis, call Exp() to handle the expression in parenthesis
        return Exp();
    else
     return atoi(&a); // if not parenthesis, return number
}

