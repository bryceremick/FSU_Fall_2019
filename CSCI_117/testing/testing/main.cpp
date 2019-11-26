//
//  main.cpp
//  testing
//
//  Created by Bryce Remick on 11/20/19.
//  Copyright © 2019 Bryce Remick. All rights reserved.
//

#include <iostream>
using namespace std;

int main(int argc, const char * argv[]) {
    
    
    int arr[]{10,12,14,16,18,20};
    int *p = arr;

//    for (int i = 0; i < 10; i++)
//        cout << *p+i << endl;
    cout << *(p + 2) << endl;
    cout << *p + 2 <<endl;
    
    
    return 0;
}

