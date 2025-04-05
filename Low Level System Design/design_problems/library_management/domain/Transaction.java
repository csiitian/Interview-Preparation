package design_problems.library_management.domain;

import java.util.Date;

public class Transaction {
  private final String transactionId;
  private final String userId;
  private final String bookBarcode;
  private final Date issueDate;
  private Date returnDate;
  private boolean isReturned;

  public Transaction(String transactionId, String userId, String bookBarcode, Date issueDate) {
    this.transactionId = transactionId;
    this.userId = userId;
    this.bookBarcode = bookBarcode;
    this.issueDate = issueDate;
    this.isReturned = false;
  }

  public String getBookBarcode() {
    return bookBarcode;
  }

  public String getUserId() {
    return userId;
  }

  public boolean isReturned() {
    return isReturned;
  }

  public void setReturned() {
    isReturned = true;
    returnDate = new Date();
  }

  @Override
  public String toString() {
    return "Transaction{" +
            "transactionId='" + transactionId + '\'' +
            ", userId='" + userId + '\'' +
            ", bookBarcode='" + bookBarcode + '\'' +
            ", issueDate=" + issueDate +
            ", returnDate=" + returnDate +
            ", isReturned=" + isReturned +
            '}';
  }
}
