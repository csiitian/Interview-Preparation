package design_problems.car_rental_system.domain;

public class Billing {
  Long billId;
  Long reservationId;
  Double amount;
  BillingStatus status;

  public Billing(Long billId, Long reservationId, Double amount, BillingStatus status) {
    this.billId = billId;
    this.reservationId = reservationId;
    this.amount = amount;
    this.status = status;
  }

  public Billing() {

  }

  public Long getBillId() {
    return billId;
  }

  public Long getReservationId() {
    return reservationId;
  }

  public BillingStatus getStatus() {
    return status;
  }

  public Double getAmount() {
    return amount;
  }

  public void confirmPayment() {
    this.status = BillingStatus.PAID;
  }
}
