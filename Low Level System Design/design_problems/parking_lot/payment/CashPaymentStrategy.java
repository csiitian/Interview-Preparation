package design_problems.parking_lot.payment;

import design_problems.parking_lot.PaymentReceipt;

public class CashPaymentStrategy implements PaymentStrategy {
    @Override
    public void processPayment(PaymentReceipt paymentReceipt) {
        System.out.println(paymentReceipt.getAmount() + " paid via cash.");
    }
}
