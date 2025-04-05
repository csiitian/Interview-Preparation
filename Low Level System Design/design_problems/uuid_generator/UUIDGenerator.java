package design_problems.uuid_generator;

import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

public class UUIDGenerator {
    private static final long EPOCH = 1700000000000L; // Custom epoch (adjustable)
    private static final int DATACENTER_BITS = 10;
    private static final int MACHINE_BITS = 10;
    private static final int SEQUENCE_BITS = 10;
    private static final int RANDOM_BITS = 57;

    private static final long MAX_DATACENTER_ID = (1L << DATACENTER_BITS) - 1;
    private static final long MAX_MACHINE_ID = (1L << MACHINE_BITS) - 1;
    private static final long MAX_SEQUENCE = (1L << SEQUENCE_BITS) - 1;

    private final long datacenterId;
    private final long machineId;
    private final AtomicLong lastTimestamp = new AtomicLong(-1L);
    private final AtomicLong sequence = new AtomicLong(0);
    private final Random random = new Random();

    public UUIDGenerator(long datacenterId, long machineId) {
        if (datacenterId > MAX_DATACENTER_ID || machineId > MAX_MACHINE_ID) {
            throw new IllegalArgumentException("Invalid datacenter/machine ID");
        }
        this.datacenterId = datacenterId;
        this.machineId = machineId;
    }

    public synchronized long generateUUID() {
        long currentTimestamp = System.currentTimeMillis() - EPOCH;
        long lastTs = lastTimestamp.get();

        if (currentTimestamp == lastTs) {
            long seq = (sequence.incrementAndGet()) & MAX_SEQUENCE;
            if (seq == 0) {
                // Wait for next millisecond
                while ((currentTimestamp = System.currentTimeMillis() - EPOCH) <= lastTs);
            }
        } else {
            sequence.set(0);
        }

        lastTimestamp.set(currentTimestamp);

        long randomBits = random.nextLong() & ((1L << RANDOM_BITS) - 1);

        return (currentTimestamp << (DATACENTER_BITS + MACHINE_BITS + SEQUENCE_BITS + RANDOM_BITS))
                | (datacenterId << (MACHINE_BITS + SEQUENCE_BITS + RANDOM_BITS))
                | (machineId << (SEQUENCE_BITS + RANDOM_BITS))
                | (sequence.get() << RANDOM_BITS)
                | randomBits;
    }

    public static void main(String[] args) {
        UUIDGenerator generator = new UUIDGenerator(1, 1);
        System.out.println(generator.generateUUID());
    }
}
