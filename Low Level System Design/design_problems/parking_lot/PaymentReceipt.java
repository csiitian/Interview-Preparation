package design_problems.parking_lot;

import java.time.LocalDateTime;
import java.util.UUID;

public class PaymentReceipt {
    private final Double amount;
    private final ParkingTicket parkingTicket;
    private final String receiptNumber = UUID.randomUUID().toString();
    private final LocalDateTime issueTime = LocalDateTime.now();

    public PaymentReceipt(Double amount, ParkingTicket parkingTicket) {
        this.amount = amount;
        this.parkingTicket = parkingTicket;
    }

    public Double getAmount() {
        return amount;
    }

    public ParkingTicket getParkingTicket() {
        return parkingTicket;
    }

    @Override
    public String toString() {
        return "PaymentReceipt{" +
                "amount=" + amount +
                ", parkingTicket=" + parkingTicket +
                ", receiptNumber='" + receiptNumber + '\'' +
                ", issueTime=" + issueTime +
                '}';
    }
}
