# 30 Days Concurrency Challenge

1. Create a Thread [ Done ]
   - Create a class and extending Thread class
   - Understand basic thread lifecycle and run method
   - start() vs run()
2. Create a Thread [ Done ]
   - Create a class implementing Runnable interface
3. Create two threads each should be able to increment a shared counter [ Done ]
   - Each thread will increment counter by 100000
   - Simulate the race condition
   - You can use synchronized or Atomic Integer
   - At the end, counter value should be correct
   - Measure the time taken by synchronized and Atomic Integer 
   - Notice the difference
4. Create two threads one print even and one print odd numbers [ Done ]
    - Both threads should print in sequence upto 100
5. Reentrant Lock vs synchronized [ Done ]
   - Handle concurrency using Reentrant Lock instead of synchronized
   - Implement a timeout while trying to acquire a lock
   - Interrupt threads waiting for lock
   - fairness
6. Deadlock Simulation [ Done ]
    - Create two resource and two thread locking each other object
    - Fix code to prevent deadlock
7. Producer Consumer Problem using wait and notify [ Done ]
   - Create a Class with Queue
   - Producer produces data until queue is not full
   - Consumer consumes data until queue is not empty
8. Reentrant Lock and Condition ( Day#4 ) [ Done ]
9. Create 5 threads incrementing shared counter alternatively [ Done ]
10. Producer Consumer using BlockingQueue [ Done ]
   - put() blocks until space available
   - take() blocks until item available

11. Semaphore [ Done ]
    - Controlled Concurrency, Only limited threads can work at same time
    - you can think of it as token, use it while working and return back once done
    - fair semaphore
12. Countdown Latch [ Done ]
    - Start 3 worker threads
    - Main thread should wait until all 3 workers finish their tasks
    - await() main thread pauses
    - countDown() worker thread signals completion
    - Once count reaches zero, cannot be reused
13. Cyclic Barrier
    - wait for other threads to reach a common point
    - only after reach the barrier, every thread proceeds together to next step
    - Example: Loading data from multiple sources, then aggregating results together
14. Phaser
    - Multi Phase synchronization
    - 3 workers doing abstract work in phases
15. ReadWriteLock
    - Read heavy systems
    - only one write thread can update ( no readers while writing )
16. StampedLock
    - Fast, modern lock with optimistic read
    - Fallback: if validation fails, acquire proper read lock
    - Best for very read-heady systems with rare writes
    - if optimistic read fails too often then no real benefit over normal read lock
17. Thread interruption
    - Create a thread simulating download
    - interrupt the thread and handle it gracefully
