# We design a 128-bit UUID optimized for scalability and uniqueness.

Structure (128 bits)
- Timestamp (41 bits) → Covers 69 years from epoch.
- Datacenter ID (10 bits) → Supports 1024 data centers globally.
- Machine ID (10 bits) → Supports 1024 machines per data center.
- Sequence ID (10 bits) → Allows 1024 unique IDs per millisecond per machine.
- Random Bits (57 bits) → Ensures uniqueness across requests.