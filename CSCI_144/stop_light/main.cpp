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
#include <queue>
#include <vector>
#include <thread>
#include <mutex>
#include <condition_variable>
using namespace std;

struct car
{
    int id;
    int time;
    string direction;
    vector<int> coverage;

    car(int i, int t, string d, vector<int> c)
    {
        id = i;
        time = t;
        direction = d;
        coverage = c;
    }
    bool operator<(const car &rhs) const
    {
        return time > rhs.time;
    }
    bool operator==(const car &rhs) const
    {
        return id == rhs.id;
    }
};

priority_queue<car> nQueue;
priority_queue<car> sQueue;
priority_queue<car> eQueue;
priority_queue<car> wQueue;
priority_queue<car> iQueue;

vector<thread> thread_vector;
condition_variable intersection;
mutex m_mutex;

void parseFile(string);
car parseCar(int,int, string);
void print_queue(priority_queue<car> &);
void go(int,int, string);

int main(int argc, const char *argv[])
{

    parseFile("difficult.txt");

    print_queue(nQueue);
    cout << "---" << endl;
    print_queue(sQueue);
    cout << "---" << endl;
    print_queue(wQueue);
    cout << "---" << endl;
    print_queue(eQueue);


    for (auto & thr : thread_vector)
        thr.join();
    

    cout << thread_vector.size() << endl;

    return 0;
}

void parseFile(string fname)
{

    ifstream file;
    int time;
    string direction;
    int id = 0;

    file.open(fname.c_str());

    if (file.is_open())
    {
        while (file >> time >> direction)
        {
            thread_vector.push_back(thread(go, id, time, direction));
            id++;
        }
    }
    else
    {
        cout << "Error: Cannot read file" << endl;
        ::exit(1);
    }
}
void go(int id, int time, string direction)
{
    unique_lock<mutex> mlock(m_mutex);

    car car = parseCar(id, time, direction);

    if (car.direction.at(0) == 'N')
    {
        nQueue.push(car);
        while(!(car == nQueue.top()))
            intersection.wait(mlock);
        
        
    }
    else if (car.direction.at(0) == 'S')
    {
        sQueue.push(car);
    }
    else if (car.direction.at(0) == 'E')
    {
        eQueue.push(car);
    }
    else if (car.direction.at(0) == 'W')
    {
        wQueue.push(car);
    }
    else
    {
        cout << "Parse Error: Not a valid direction" << endl;
        ::exit(1);
    }
}
car parseCar(int id, int time, string dir)
{
    if (dir.compare("N") == 0)
    {
        vector<int> cov{1, 3};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("NE") == 0)
    {
        vector<int> cov{3};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("NW") == 0)
    {
        vector<int> cov{0, 3};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("S") == 0)
    {
        vector<int> cov{0, 2};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("SE") == 0)
    {
        vector<int> cov{0, 3};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("SW") == 0)
    {
        vector<int> cov{0};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("E") == 0)
    {
        vector<int> cov{2, 3};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("EN") == 0)
    {
        vector<int> cov{1, 2};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("ES") == 0)
    {
        vector<int> cov{2};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("W") == 0)
    {
        vector<int> cov{0, 1};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("WS") == 0)
    {
        vector<int> cov{1, 2};
        return car(id, time, dir, cov);
    }
    else if (dir.compare("WN") == 0)
    {
        vector<int> cov{1};
        return car(id, time, dir, cov);
    }
    else
    {
        cout << "Parse Error: Not a valid direction" << endl;
        ::exit(1);
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
