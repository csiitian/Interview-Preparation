package design_problems.food_delivery_system.domain;

public interface IPrototype extends Cloneable {
    Object clone() throws CloneNotSupportedException;
}
