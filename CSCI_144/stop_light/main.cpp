//
//  main.cpp
//  stop_light_bryceremick
//
//  Created by Bryce Remick on 11/14/19.
//  Copyright © 2019 Bryce Remick. All rights reserved.
//

#include <iostream>
#include <cstdlib>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <cctype>
#include <string>
#include <iomanip>
#include <vector>
#include <queue>
using namespace std;

struct car {
    int time;
    string direction;
    vector<int> coverage;
    
    car(int t, string d, vector<int> c){
        time = t;
        direction = d;
        coverage = c;
    }
    bool operator<(const car &rhs) const
    {
        return time > rhs.time;
    }
};

priority_queue<car> nQueue;
priority_queue<car> sQueue;
priority_queue<car> eQueue;
priority_queue<car> wQueue;
priority_queue<car> iQueue;

void parseFile(string);
car parseCar(int, string);
void print_queue(priority_queue<car> &);

int main(int argc, const char * argv[]) {
    
    parseFile("difficult.txt");
    
    print_queue(nQueue);
    cout << "---" << endl;
    print_queue(sQueue);
    cout << "---" << endl;
    print_queue(wQueue);
    cout << "---" << endl;
    print_queue(eQueue);
    
    
    return 0;
}

void parseFile(string fname){
    
    ifstream file;
    int time;
    string direction;
    
    file.open(fname);
    
    if (file.is_open())
    {
        while (file >> time >> direction)
        {
            car car = parseCar(time, direction);
            
            if (car.direction.at(0) == 'N'){
                nQueue.push(car);
            }
            else if (car.direction.at(0) == 'S'){
                sQueue.push(car);
            }
            else if (car.direction.at(0) == 'E'){
                eQueue.push(car);
            }
            else if (car.direction.at(0) == 'W'){
                wQueue.push(car);
            }
            else{
                cout << "Parse Error: Not a valid direction" << endl;
                exit(1);
            }
        }
    }
    else{
        cout << "Error: Cannot read file" << endl;
        exit(1);
    }
        
}
car parseCar(int time, string dir){
    if (dir.compare("N") == 0){
        vector<int> cov {1,3};
        return car(time, dir, cov);
    }
    else if (dir.compare("NE") == 0){
        vector<int> cov {3};
        return car(time, dir, cov);
    }
    else if (dir.compare("NW") == 0){
        vector<int> cov {0,3};
        return car(time, dir, cov);
    }
    else if (dir.compare("S") == 0){
        vector<int> cov {0,2};
        return car(time, dir, cov);
    }
    else if (dir.compare("SE") == 0){
        vector<int> cov {0,3};
        return car(time, dir, cov);
    }
    else if (dir.compare("SW") == 0){
        vector<int> cov {0};
        return car(time, dir, cov);
    }
    else if (dir.compare("E") == 0){
        vector<int> cov {2,3};
        return car(time, dir, cov);
    }
    else if (dir.compare("EN") == 0){
        vector<int> cov {1,2};
        return car(time, dir, cov);
    }
    else if (dir.compare("ES") == 0){
        vector<int> cov {2};
        return car(time, dir, cov);
    }
    else if (dir.compare("W") == 0){
        vector<int> cov {0,1};
        return car(time, dir, cov);
    }
    else if (dir.compare("WS") == 0){
        vector<int> cov {1,2};
        return car(time, dir, cov);
    }
    else if (dir.compare("WN") == 0){
        vector<int> cov {1};
        return car(time, dir, cov);
    }else{
        cout << "Parse Error: Not a valid direction" << endl;
        exit(1);
    }
}


void print_queue(priority_queue<car> &pq)
{
    while (!pq.empty())
        {
            cout << setw(4) << pq.top().time << setw(4) << pq.top().direction << setw(4) << endl;
            pq.pop();
        }
}
