package design_problems.car_rental_system.domain;

public class Payment {
  Long paymentId;
  Long billingId;
  String transactionId;
  PaymentType type;
  Double amount;
  PaymentStatus status;

  public Payment(Long paymentId, Long billingId, String transactionId, PaymentType type, Double amount, PaymentStatus status) {
    this.paymentId = paymentId;
    this.billingId = billingId;
    this.transactionId = transactionId;
    this.type = type;
    this.amount = amount;
    this.status = status;
  }

  public Payment() {

  }

  public void setPaymentStatus(PaymentStatus paymentStatus) {
    this.status = paymentStatus;
  }

  public Long getPaymentId() {
    return paymentId;
  }

  public String getTransactionId() {
    return transactionId;
  }

  public Long getBillingId() {
    return billingId;
  }

  public PaymentType getType() {
    return type;
  }

  public Double getAmount() {
    return amount;
  }

  public PaymentStatus getStatus() {
    return status;
  }
}
