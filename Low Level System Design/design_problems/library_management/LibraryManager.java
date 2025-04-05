package design_problems.library_management;

import design_problems.library_management.domain.*;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class LibraryManager {
  List<User> users;
  List<Book> books;
  List<Author> authors;
  List<Publisher> publishers;
  List<Transaction> transactions;
  Map<String, Cart> userCartMap;

  LibraryManager() {
    users = new ArrayList<>();
    books = new ArrayList<>();
    authors = new ArrayList<>();
    publishers = new ArrayList<>();
    transactions = new ArrayList<>();
    userCartMap = new HashMap<>();
  }

  public void addUser(User user) {
    users.add(user);
  }

  public void addBook(Book book) {
    books.add(book);
  }

  public void addBookItem(Book book, BookItem bookItem) {
    book.addBookItem(bookItem);
  }

  public void addAuthor(Author author) {
    authors.add(author);
  }

  public void addPublisher(Publisher publisher) {
    publishers.add(publisher);
  }

  public void blockUser(User user) {
    user.blockUser();
  }

  public void addBookItemToCart(User user, BookItem bookItem) {
    Cart cart = userCartMap.computeIfAbsent(user.getUserId(), k -> new Cart());
    // check if book is available and no same book copy is already in cart
    if (bookItem.isBorrowed()) {
      System.out.println("Book with ISBN: " + bookItem.getBook().getISBN() + " is already borrowed");
      return;
    }
    // check ISBN is not present
    for (CartItem cartItem : cart.getCartItems()) {
      if (cartItem.getBookItem().getBook().getISBN().equals(bookItem.getBook().getISBN())) {
        System.out.println("Book with ISBN: " + bookItem.getBook().getISBN() + " is already in cart");
        return;
      }
    }
    cart.addItem(bookItem);
  }

  public void checkout(User user) {
    Cart cart = userCartMap.get(user.getUserId());
    if (cart == null) {
      System.out.println("No items in cart for user with id: " + user.getUserId());
      return;
    }
    List<Transaction> userTransactions = new ArrayList<>();
    for (CartItem cartItem : cart.getCartItems()) {
      if (!cartItem.getBookItem().checkout()) {
        return;
      }
      Transaction transaction = new Transaction("T" + System.currentTimeMillis(), user.getUserId(), cartItem.getBookItem().getBarcode(), new Date());
      userTransactions.add(transaction);
    }
    cart.getCartItems().clear();
    cart.setStatus(CartStatus.CLOSED);
    transactions.addAll(userTransactions);
    System.out.println("Books issued successfully to user with id: " + user.getUserId() + " and transaction ids: " + userTransactions);
  }

  public void returnBooks(List<String> barCodes) {
    Set<String> barCodeSets = new HashSet<>(barCodes);
    for (Transaction transaction : transactions) {
      if (barCodeSets.contains(transaction.getBookBarcode()) && !transaction.isReturned()) {
        transaction.setReturned();
        System.out.println("Book returned successfully by user with id: " + transaction.getUserId() + " and transaction: " + transaction);
      }
    }
  }

  public List<Transaction> getUserTransactions(User user) {
    List<Transaction> userTransactions = new ArrayList<>();
    for (Transaction transaction : transactions) {
      if (transaction.getUserId().equals(user.getUserId())) {
        userTransactions.add(transaction);
      }
    }
    return userTransactions;
  }
}
