## How ThreadPoolExecutor Works Internally?

### 🔹 Key Components:
- Core Pool Size – Minimum threads kept alive.
- Maximum Pool Size – Upper limit of threads.

- Blocking Queue – Stores tasks if all thread are busy.
- Keep-Alive Time – How long idle threads stay alive.

### 🔹 Execution Flow:
1️⃣ Task submitted via execute() or submit().

2️⃣ If pool size < corePoolSize, a new thread is created.

3️⃣ If pool size ≥ corePoolSize, task goes to queue.

4️⃣ If queue is full, a new thread is created until maxPoolSize.

5️⃣ If threads exceed maxPoolSize, RejectedExecutionHandler is triggered.