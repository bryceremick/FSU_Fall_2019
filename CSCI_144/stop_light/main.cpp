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
#include <chrono>
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
queue<car> iQueue;

vector<thread> thread_vector;
vector<bool> ready_vector; 
condition_variable intersection;
mutex m_mutex;
char globDirection;

void parseFile(string);
car parseCar(int, int, string);
void print_queue(priority_queue<car> &);
void go(int, int, string);
void centralProcessing();
bool collision(car);
bool allQueuesEmpty();
bool isSubset(vector<int>, vector<int>);
void release(int);

int main(int argc, const char *argv[])
{

    globDirection = 'N';
    parseFile("difficult.txt");                 // parse file and create all threads

    thread mainThread(centralProcessing);       // start central processing thread

    for (auto &thr : thread_vector)             // join threads
        thr.join();

    mainThread.join();                          // join main thread


    cout << "DONE" << endl;
    return 0;
}

void parseFile(string fname)
{

    ifstream file;
    int time;
    string direction;
    int id = 0;

    file.open(fname.c_str());

    if (file.is_open())     // if file is open...
    {
        while (file >> time >> direction)   // read line by line
        {
            thread_vector.push_back(thread(go, id, time, direction));   // create car thread
            ready_vector.push_back(false);                              // push back "false" into ready vector
            id++;
        }
    }
    else        // else error with file
    {
        cout << "Error: Cannot read file" << endl;
        ::exit(1);
    }
}
void go(int id, int time, string direction)
{
    unique_lock<mutex> mlock(m_mutex);
    priority_queue<car> *thisQueue;

    car car = parseCar(id, time, direction);

    char releaseDirection = car.direction.at(0);

    if (releaseDirection == 'N')
    {
        nQueue.push(car);           // push car into correct queue
        thisQueue = &nQueue;        // assign thisQueue pointer for later use
    }
    else if (releaseDirection == 'S')
    {
        sQueue.push(car);
        thisQueue = &sQueue;
    }
    else if (releaseDirection == 'E')
    {
        eQueue.push(car);
        thisQueue = &eQueue;
    }
    else if (releaseDirection == 'W')
    {
        wQueue.push(car);
        thisQueue = &wQueue;
    }
    else
    {
        cout << "Parse Error: Not a valid direction" << endl;
        ::exit(1);
    }

     while(!ready_vector.at(car.id))     // wait until allowed to enter intersection
    {
        cout << "car_" << car.id << " waiting" << endl;
        intersection.wait(mlock);
    }

    iQueue.push(thisQueue->top());
    cout << "iqueue size:" << iQueue.size() << endl;
    thisQueue->pop();

    this_thread::sleep_for(chrono::milliseconds(100)); 
    cout << "Car_" << iQueue.front().id << " is leaving " << iQueue.front().direction.at(0) << "Queue" << endl;
    iQueue.pop();

    thisQueue = NULL;
    delete thisQueue;
}
void centralProcessing()
{
    // unique_lock<mutex> mlock(m_mutex);
    priority_queue<car> *currQueue;
    char currDirection;

    while (!(allQueuesEmpty()))
    {
        for (int i = 0; i < 4; i++)
        {
            if (i == 0)
            {
                currQueue = &nQueue;
                currDirection = 'N';
            }
            else if (i == 1)
            {
                currQueue = &eQueue;
                currDirection = 'E';
            }
            else if (i == 2)
            {
                currQueue = &sQueue;
                currDirection = 'S';
            }
            else
            {
                currQueue = &wQueue;
                currDirection = 'W';
            }

            if (!(collision(currQueue->top())))
            {
                // ready_vector.at(currQueue->top().id) = true;
                // intersection.notify_one();
                release(currQueue->top().id);
                // cout << "no collision between car_" << currQueue->top().id << " and iQueue" << endl;
            }

            this_thread::sleep_for(chrono::milliseconds(200));
        }
    }
    currQueue = NULL;
    delete currQueue;
}
void release(int id)
{
    unique_lock<mutex> mlock(m_mutex);
    ready_vector.at(id) = true;
    intersection.notify_one();
}
bool collision(car thisCar)
{

    // temp intersection queue (to loop through and pop)
    queue<car> tempIQueue = iQueue;

    // comparison vectors (for diagnol turns)
    vector<int> cmprVec1{0, 3};
    vector<int> cmprVec2{1, 2};

    // while intersection queue is not empty...
    while (!(tempIQueue.empty()))
    {

        if (thisCar.coverage == cmprVec1 && tempIQueue.front().coverage == cmprVec1) // if both cars have 0,3 diagnal turns
            continue;
        else if (thisCar.coverage == cmprVec2 && tempIQueue.front().coverage == cmprVec2) // if both cars have 1,2 diagnol turns
            continue;
        else if (isSubset(thisCar.coverage, tempIQueue.front().coverage)) // if the two cars share a coverage quadrant
            return true;
        else
            continue; // no collision

        tempIQueue.pop(); // pop and move to next car in intersection queue
    }

    return false; // if control reaches here, no collisions
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
bool allQueuesEmpty()
{
    return nQueue.empty() && sQueue.empty() && eQueue.empty() && wQueue.empty();
}
bool isSubset(vector<int> A, vector<int> B)
{
    for (int i = 0; i < A.size(); i++)
        for (int j = 0; j < B.size(); j++)
            if (A[i] == B[j])
                return true;

    return false;
}