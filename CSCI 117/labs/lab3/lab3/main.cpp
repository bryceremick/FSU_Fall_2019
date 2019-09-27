//
//  Lab3 (CONTINUED FROM LAB 2)
//  CSCI 117 - Spring 2019
//
//  Created by Bryce Remick on 9/17/19.
//  Copyright © 2019 Bryce Remick. All rights reserved.
//

#include <iostream>
#include <cstdlib> //for atoi() #include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <cctype>
#include <string>
#include <iomanip>
#include <vector>
using namespace std;

int Exp(), Term(), Exp2(int), Term2(int), Fact(), Fact2(int), Num(), Expression(string);
void Declarations(), Statements(), Declaration(string), Statement(string), Assign_state(string), Print_state(), declareVar(string, string), print_table();
ifstream file;
string prog;    //string for reading 1-line input expression (program) int indexx = 0; //global index for program string
int indexx = 0; //global index for program string

// function to strip whitespaces from input string
void stripWhitespaces()
{
    prog.erase(std::remove(prog.begin(), prog.end(), ' '),
               prog.end());
}

// sym_table struct
struct node
{
    char id;
    string type;
    double value;
};
// members (for searching sym_table)
bool operator==(const node &l, const node &r)
{
    return l.id == r.id;
}

//node sym_table[100];
vector<node> sym_table;

int main(int argc, const char **argv)
{
    
    string word;
    file.open("data1.txt");
    
    if (file.is_open())
    {
        file >> word;   // read first word
        if (word == "program")
        {
            Declarations();     // do declarations
            Statements();       // do statements after declarations
        }
        else
        {
            cout << "Error: Program does not start with 'program' keyword" << endl;
            exit(1);
        }
    }
    else
        cout << "Error: Cannot read file" << endl;
    
    print_table(); // print sym_table
}

void Declarations()
{
    string word;
    file >> word;       // read a word
    
    if (word == "begin")      // if word is begin, return and start doing delcarations
        return;
    else if (word == "int" || word == "double")     // otherwise, handle declaration
        Declaration(word);
    
    Declarations();     // repeat for all declarations
}
void Statements()
{
    string word;
    file >> word;       // read a word
    
    if (word == "end")      // if end of program, return
        return;
    else
        Statement(word);    // otherwise, handle a statements
    
    Statements();       // repeat for all statements
}

void Declaration(string word)
{
    if (word == "int")                  // if int declaration
    {
        file >> word;                       // read next word
        while (word.at(1) != ';')           // keep reading declarations until semi colon
        {
            declareVar(word, "int");        // init each declaration in sym table
            file >> word;                   // read next word
        }
        declareVar(word, "int");            // init last declaration in sym table
    }
    else if (word == "double")          // if double declaration
    {
        file >> word;                       // read next word
        while (word.at(1) != ';')           // keep reading declarations until semicolon
        {
            declareVar(word, "double");     // init each declaration in sym table
            file >> word;                   // read next word
        }
        declareVar(word, "double");         // init last declaration in symtable
    }
}

void Statement(string word)
{
    if (word == "print")            // if word is "print"
        Print_state();              // print value of variable
    else
        Assign_state(word);         // else, assaign value TO variable
}

/******************* Assign_state **************************
 word = identifier left of '=' (passed in)
 word2 = temp variable to store each "word" in statement while looping
 expr = full expression that exists on the right of the '='
 --
 it = reference to left side identifier in sym_table
 it2 = reference to right side identifier in sym_table
 --
 *********************************************************/
void Assign_state(string word)
{
    string word2;  // temp var to store words while looping
    file >> word2; // read next word in statement
    string expr;   // var to store FULL expression (everything right side of '=')
    
    if (word2 == "=") // if next word is '='
    {
        vector<node>::iterator it, it2; // declare vector iterators for searching
        node node, node2;               // TEMP nodes used for seaching (find function)
        
        // search for identifier left of '=' char
        node.id = word.at(0);
        it = find(sym_table.begin(), sym_table.end(), node);
        
        // if identifier not found...
        if (it == sym_table.end())
        {
            cout << "undeclared variable: " << word << endl;
            exit(1);
        }
        
        file >> word2;              // read next word in statement
        expr = word2;               // init expression with first word in statement
        while (word2.back() != ';') // loop until end of statement/expression
        {
            file >> word2; // get next word in expression
            expr += word2; // append to expression
        }
        
        expr = expr.substr(0, expr.size() - 1); // remove semicolon from expr
        
        if (expr.length() == 1) // if expr is single char (letter or number)
        {
            if (isalpha(expr.at(0))) // if single char is alpha (identifier)
            {
                // lookup identifier in sym_table
                node2.id = expr.at(0);
                it2 = find(sym_table.begin(), sym_table.end(), node2);
                
                // if identifier is found, assign it's value to the identifier left of '=' (it)
                if (it2 != sym_table.end())
                    it->value = it2->value;
                else
                {
                    cout << "undeclared value: " << expr << endl;
                    exit(1);
                }
            }
            else                              // else single char is digit
                it->value = Expression(expr); // evaluate expression and assign to left side identifier
        }
        else                              // else expr is multi char
            it->value = Expression(expr); // evaluate expression and assign to left side identifier
    }
}
void Print_state()
{
    string word;
    file >> word; // read next word (after "print")
    
    if (isalpha(word.at(0))) // if first char in word is alpha..
    {
        
        // search for identifier in sym_table (using iterator)
        vector<node>::iterator it;
        node node;
        node.id = word.at(0);
        it = find(sym_table.begin(), sym_table.end(), node);
        
        // if identifier is found, display the value of it
        if (it != sym_table.end())
            cout << it->value << endl;
        else
        {
            cout << "undeclared value: " << word.at(0) << endl;
            exit(1);
        }
    }
    else // else first char in word is NOT alpha.. (assume it's expression)
    {
        string expr = word;        // store word
        while (word.back() != ';') // loop until end of expression
        {
            file >> word; // get next word in expression
            expr += word; // append to expression var
        }
        expr = expr.substr(0, expr.size() - 1); // remove semicolon from expr
        
        cout << Expression(expr) << endl; // evaluate and display expression
    }
}

/******************** Helper Funcs **********************/
void print_table()
{
    cout << "+----------Symbol Table-----------+" << endl;
    for (const node &var : sym_table)
        cout << "| " << var.id << " | " << setw(8) << var.type << " | " << setw(16) << var.value << " | " << endl;
    cout << "+---------------------------------+" << endl;
}
void declareVar(string word, string type)
{
    node node;
    node.id = word.at(0);
    node.type = type;
    sym_table.push_back(node);
}

int Expression(string expr)
{
    prog = expr;
    indexx = 0;
    stripWhitespaces();
    return Exp();
}
/*****************************************************/

/***************** Lab2 Expression Handling ************/
int Exp()
{
    return Exp2(Term());
}
int Term()
{
    return Term2(Fact());
}

int Exp2(int inp)
{
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

int Term2(int inp)
{
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
    return Fact2(Num());
}
int Fact2(int inp)
{
    int result = inp;
    
    if (indexx < prog.length())
    {
        char a = prog.at(indexx++); //get one chr from program string
        
        if (a == '^')
            result = Fact2(pow(result, Fact())); // recursive power operator
        else
            indexx--; // backtrack if not Fact2
    }
    return result;
}

int Num()
{
    char a = prog.at(indexx++); //get one chr from program string
    
    // if beginning of parenthesis, call Exp() to handle the expression in parenthesis
    if (a == '(')
        return Exp();
    
    string num_str = "";
    
    // while current char (a) is a digit..check to see if next char in string is a digit.
    while (isdigit(a) && (indexx < prog.length()))
    {
        num_str += a;
        a = prog.at(indexx++);
    }
    
    indexx--;
    num_str += a;
    
    return atoi(&num_str.at(0)); // if not parenthesis, return number
}
