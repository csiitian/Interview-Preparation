package design_problems.parking_lot.payment;

import design_problems.parking_lot.PaymentReceipt;

public interface PaymentStrategy {
    void processPayment(PaymentReceipt paymentReceipt);
}
