package design_problems.library_management;

import design_problems.library_management.domain.*;

import java.util.List;

public class MainApplication {

  public static void main(String[] args) {
    LibraryManager libraryManager = new LibraryManager();
    Author author = new Author("Author1", "author1@gmail.com");
    Publisher publisher = new Publisher("Publisher1", "address1", "12345");
    Book book1 = new Book("1234", "title", "subject", publisher, "English", 15, author);
    Book book2 = new Book("1235", "title", "subject", publisher, "English", 15, author);
    BookItem bookItem1 = new BookItem(book1, "1234-12341");
    BookItem bookItem2 = new BookItem(book1, "1234-12342");
    BookItem bookItem3 = new BookItem(book2, "1235-12341");
    User user = new User("123", "user1", "user@gmail.com", "12345", UserType.CUSTOMER, UserStatus.ACTIVE);

    libraryManager.addAuthor(author);
    libraryManager.addPublisher(publisher);
    libraryManager.addBook(book1);
    libraryManager.addBookItem(book1, bookItem1);
    libraryManager.addUser(user);

    // add items in cart
    libraryManager.addBookItemToCart(user, bookItem1);
    libraryManager.addBookItemToCart(user, bookItem2);
    libraryManager.addBookItemToCart(user, bookItem3);
    // checkout
    libraryManager.checkout(user);

    // return books
    libraryManager.returnBooks(List.of("1234-12341"));

    // get user transactions
    List<Transaction> userTransactions = libraryManager.getUserTransactions(user);
    System.out.println("User: " + user + " Transactions: " + userTransactions);
  }
}
