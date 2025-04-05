package design_problems.library_management.domain;

import java.util.concurrent.locks.ReentrantLock;

public class BookItem {
  Book book;
  String barcode;
  boolean isReferenceOnly;
  boolean isBorrowed;
  Long borrowed;
  Long dueDate;
  BookFormat format;

  public BookItem(Book book, String barcode, boolean isReferenceOnly, Long borrowed, Long dueDate, BookFormat format) {
    this.book = book;
    this.barcode = barcode;
    this.isReferenceOnly = isReferenceOnly;
    this.borrowed = borrowed;
    this.dueDate = dueDate;
    this.format = format;
  }

  public BookItem(Book book, String barCode) {
    this.book = book;
    this.barcode = barCode;
    this.isReferenceOnly = false;
    this.isBorrowed = false;
    this.borrowed = -1L;
    this.dueDate = -1L;
  }

  public String getBarcode() {
    return barcode;
  }

  public Book getBook() {
    return book;
  }

  public boolean isBorrowed() {
    return isBorrowed;
  }

  public boolean checkout() {
    if (isReferenceOnly) {
      System.out.println("This book is for reference only and can't be issued");
      return false;
    }
    if (isBorrowed) {
      System.out.println("This book is already borrowed");
      return false;
    }
    isBorrowed = true;
    borrowed = System.currentTimeMillis();
    dueDate = borrowed + 14 * 24 * 60 * 60 * 1000;
    return true;
  }
}
