//
//  main.cpp
//  stop_light_bryceremick
//
//  Created by Bryce Remick on 11/14/19.
//  CSCI 144, Fall 2019
//  Copyright © 2019 Bryce Remick. All rights reserved.
//

#include <iostream>
#include <cstdlib>
#include <fstream>
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
    bool operator<(const car &rhs) const    // overload less than operator (for priority queue)
    {
        return time > rhs.time;
    }
    bool operator==(const car &rhs) const   // overload equals operator
    {
        return id == rhs.id;
    }
};

priority_queue<car> nQueue;
priority_queue<car> sQueue;
priority_queue<car> eQueue;
priority_queue<car> wQueue;
queue<car> iQueue;

vector<thread> thread_vector;           // contains all car threads
vector<bool> ready_vector;              // associated with thead_vector
condition_variable intersection;        // intersection CV
mutex m_mutex;


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
    chrono::time_point<chrono::system_clock> start, end;
    start = chrono::system_clock::now();        // start timing

    parseFile("difficult.txt");                 // parse file and create all threads
    thread mainThread(centralProcessing);       // start central processing thread

    for (auto &thr : thread_vector)             // join threads
        thr.join();

    mainThread.join();                          // join main thread
    
    end = chrono::system_clock::now();          // end timing
    chrono::duration<double> elapsed_seconds = end - start; 

    cout << "DONE: " << elapsed_seconds.count() << " seconds" << endl;
    return 0;
}

// function that takes in a file name as a paramter, and reads it line by line
// creating threads for each line (car)
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
// thread call back function
// pushes car in appropriate queue, and then waits until car is allowed to enter intersection
void go(int id, int time, string direction)
{
    unique_lock<mutex> mlock(m_mutex);
    priority_queue<car> *thisQueue;

    car car = parseCar(id, time, direction);

    char releaseDirection = car.direction.at(0);

    if (releaseDirection == 'N')    // if car is heading north...
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
        intersection.wait(mlock);

    iQueue.push(thisQueue->top());      // push car into intersection
    thisQueue->pop();                   // pop car from original queue

    this_thread::sleep_for(chrono::milliseconds(100));  // sleep (simulate driving)
    
    // print car direction
    if (iQueue.front().direction.size() < 2)
    {
        cout << iQueue.front().time << setw(10) << "Car_" << iQueue.front().id << " is heading " << 
        iQueue.front().direction.at(0)<< endl;
    }
    else
    {
        cout << iQueue.front().time << setw(10) << "Car_" << iQueue.front().id << " heading " << 
        iQueue.front().direction.at(0) << " turned " << iQueue.front().direction.at(1) << endl;
    }
    
    // pop car from intersection queue
    iQueue.pop();

    // delete pointer
    thisQueue = NULL;
    delete thisQueue;
}
// thread callback function that handles the central processing of the intersection.
// Looks at top of each direction queue, and compares that car to all cars in intersection queue.
// If there are no collisions, signal that the car from the direction queue may enter intersection.
void centralProcessing()
{
    priority_queue<car> *currQueue;     // pointer to current working queue
    char currDirection;

    while (!(allQueuesEmpty()))         // while all direction queues are not empty...
    {
        for (int i = 0; i < 4; i++)     // iterate over the top of each direction queue
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

            // if NO collision between top car of direction queue and all cars in intersection queue...
            if (!(collision(currQueue->top())))
                release(currQueue->top().id);   // release(signal) car

            this_thread::sleep_for(chrono::milliseconds(200));  // sleep
        }
    }

    // delete pointer
    currQueue = NULL;
    delete currQueue;
}
// function that releases(notifies) a car to enter the intersection
void release(int id)
{
    unique_lock<mutex> mlock(m_mutex);
    ready_vector.at(id) = true;     // specify which car is allowed to enter intersection
    intersection.notify_one();      // notify
}
// function that checks for collisions between car that is passed in, and all cars in 
// intersection queue
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
            continue;      // no collision
        else if (thisCar.coverage == cmprVec2 && tempIQueue.front().coverage == cmprVec2) // if both cars have 1,2 diagnal turns
            continue;       // no collision
        else if (isSubset(thisCar.coverage, tempIQueue.front().coverage)) // if the two cars share a coverage quadrant
            return true;    // collision
        else
            continue;       // no collision

        tempIQueue.pop(); // pop and move to next car in intersection queue
    }

    return false; // if control reaches here, no collisions
}
// function that takes in car details, and parses the car by determining the collision coverage.
// takes in car id, car time, and car direction as parameters.
// returns the constructed car object.
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
// function to print a priority queue
void print_queue(priority_queue<car> &pq)
{
    while (!pq.empty())
    {
        cout << setw(4) << pq.top().time << setw(4) << pq.top().direction << setw(4) << endl;
        pq.pop();
    }
}
// function that specifies whether or not all the direction queues are empty
bool allQueuesEmpty()
{
    return nQueue.empty() && sQueue.empty() && eQueue.empty() && wQueue.empty();
}
// function that checks two cars coverage vectors for similar values (quadrants)
// if the two vectors share a similar value, return true. otherwise, false.
bool isSubset(vector<int> A, vector<int> B)
{
    for (int i = 0; i < A.size(); i++)
        for (int j = 0; j < B.size(); j++)
            if (A[i] == B[j])
                return true;

    return false;
}