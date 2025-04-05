package design_problems.library_management.domain;

public class CartItem {
  private final BookItem bookItem;
  private Long addedDate;

  public CartItem(BookItem bookItem) {
    this.bookItem = bookItem;
    this.addedDate = System.currentTimeMillis();
  }

  public BookItem getBookItem() {
    return bookItem;
  }
}
