package design_problems.jackpot_machine.domain;

import java.util.Random;

public enum Symbol {
    CHERRY(2),
//    LEMON(3),
//    ORANGE(4),
//    BELL(5),
//    SEVEN(10),
    BAR(15); // Highest payout

    private final int multiplier;

    Symbol(int multiplier) {
        this.multiplier = multiplier;
    }

    public int getMultiplier() {
        return multiplier;
    }

    public static Symbol getRandomSymbol() {
        Symbol[] symbols = Symbol.values();
        Random random = new Random();
        return symbols[random.nextInt(symbols.length)];
    }
}
