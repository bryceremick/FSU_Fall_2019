//frame code for programming assignment 5;
//file name must be same as the main class name, i.e., sr_manager.java

public class sr_manager // main class
{
    static sr1 r1 = new sr1();
    static sr2 r2 = new sr2();

    public static void main(String args[]) {
        // create and start 6 threads for 6 concurrent processes

        for (int i = 0; i < 6; i++) {
            process process = new process(i, r1, r2);
            process.start();
        }
    }
}

class sr1 // class for shared resource r1
{
    private static int avail1 = 3;

    // synchronized method cannot be interrupted;
    // only one thread can access at a time;
    public synchronized void acquire(int id) throws InterruptedException {
        if (avail1 == 0) {
            wait();
            System.out.println("+process_" + id + " is waiting for SR1");
        }
        System.out.println("process_" + id + " acquires SR1");
        avail1--;
    }

    // synchronized method cannot be interrupted;
    // only one thread can access at a time;
    public synchronized void release(int id) {
        System.out.println("process_" + id + " releases SR1");
        avail1++;
        if (avail1 == 1) {
            notify();
        }
    }
}// class sr1

class sr2 // class for shared resource r2
{
    private static int avail2 = 2;

    // synchronized method cannot be interrupted;
    // only one thread can access at a time;
    public synchronized void acquire(int id) throws InterruptedException {
        if (avail2 == 0) {
            wait();
            System.out.println("+process_" + id + " is waiting for SR2");
        }
        System.out.println("process_" + id + " acquires SR2");
        avail2--;
    }

    // synchronized method cannot be interrupted;
    // only one thread can access at a time;
    public synchronized void release(int id) {
        System.out.println("process_" + id + " releases SR2");
        avail2++;
        if (avail2 == 1) {
            notify();
        }
    }
}// class sr2

class process extends Thread // thread class name is "process"
{
    static sr1 r1;
    static sr2 r2;
    private int id;

    public process(int k, sr1 r1, sr2 r2) // constructor
    {
        System.out.println("======= Thread for process_" + k + " created");
        this.r1 = r1;
        this.r2 = r2;
        this.id = k;
    }

    public void run() {
        // acquire r1 and r2;
        // display "process_i is working";
        // release r1 and r2;

        try {
            Thread.sleep(500);
            r1.acquire(id);
            Thread.sleep(500);
            r2.acquire(id);
            System.out.println("---- process_" + id + " is working");
            Thread.sleep(500);
            r1.release(id);
            Thread.sleep(500);
            r2.release(id);

        } catch (InterruptedException e) {
            System.out.println("Exception: " + e);
        }
    }
}// class process

/*
======= Thread for process_0 created
======= Thread for process_1 created
======= Thread for process_2 created
======= Thread for process_3 created
======= Thread for process_4 created
======= Thread for process_5 created
process_0 acquires SR1
process_5 acquires SR1
process_4 acquires SR1
process_5 acquires SR2
process_4 acquires SR2
---- process_5 is working
---- process_4 is working
process_5 releases SR1
+process_2 is waiting for SR1
process_2 acquires SR1
process_4 releases SR1
+process_1 is waiting for SR1
process_1 acquires SR1
process_5 releases SR2
+process_0 is waiting for SR2
process_0 acquires SR2
---- process_0 is working
process_4 releases SR2
process_2 acquires SR2
---- process_2 is working
+process_1 is waiting for SR2
process_1 acquires SR2
---- process_1 is working
process_0 releases SR1
+process_3 is waiting for SR1
process_3 acquires SR1
process_1 releases SR1
process_2 releases SR1
process_2 releases SR2
process_0 releases SR2
process_3 acquires SR2
---- process_3 is working
process_1 releases SR2
process_3 releases SR1
process_3 releases SR2

*/
