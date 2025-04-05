package design_problems.library_management.domain;

import java.util.HashSet;
import java.util.Set;

public class Cart {
  private int cartId;
  private int userId;
  private final Set<CartItem> cartItems;
  private CartStatus status;

  public Cart() {
    cartItems = new HashSet<>();
    status = CartStatus.OPEN;
  }

  public void addItem(BookItem bookItem) {
    CartItem cartItem = new CartItem(bookItem);
    cartItems.add(cartItem);
  }

  public boolean removeItem(CartItem cartItem) {
    return cartItems.remove(cartItem);
  }

  public Set<CartItem> getCartItems() {
    return cartItems;
  }

  public void setStatus(CartStatus status) {
    this.status = status;
  }
}
