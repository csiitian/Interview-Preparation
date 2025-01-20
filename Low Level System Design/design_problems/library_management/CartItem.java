package design_problems.library_management;

public class CartItem {
  private BookItem bookItem;
  private Long addedDate;

  public CartItem(BookItem bookItem) {
    this.bookItem = bookItem;
    this.addedDate = System.currentTimeMillis();
  }

  public BookItem getBookItem() {
    return bookItem;
  }
}
