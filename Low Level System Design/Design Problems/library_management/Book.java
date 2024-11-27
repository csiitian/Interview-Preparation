package system_design.library_management;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Book {
  private String ISBN;
  private String title;
  private String subject;
  private Publisher publisher;
  private String language;
  private int numberOfPages;
  private Author author;
  private Set<BookItem> bookItems;

  public Book(String ISBN, String title, String subject, Publisher publisher, String language,
      int numberOfPages, Author author) {
    this.ISBN = ISBN;
    this.title = title;
    this.subject = subject;
    this.publisher = publisher;
    this.language = language;
    this.numberOfPages = numberOfPages;
    this.author = author;
    bookItems = new HashSet<>();
  }

  public boolean addBookItem(BookItem bookItem) {
    bookItems.add(bookItem);
    return true;
  }

  public String getISBN() {
    return ISBN;
  }
}
