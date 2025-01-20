package design_problems.library_management;

import java.util.HashSet;
import java.util.Set;

public class Cart {
  private int cartId;
  private int userId;
  private Set<CartItem> cartItems;
  private CartStatus status;

  Cart() {
    cartItems = new HashSet<>();
    status = CartStatus.OPEN;
  }

  public boolean addItem(BookItem bookItem) {
    CartItem cartItem = new CartItem(bookItem);
    return cartItems.add(cartItem);
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
