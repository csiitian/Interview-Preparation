package design_problems.jackpot_machine;

import design_problems.jackpot_machine.domain.Symbol;

public class SlotMachine {
    private final int MAX_REELS = 3;
    private final int MAX_BONUS_SPINS = 2; // follow-up question
    private final int BASE_BET = 10;
    private Symbol[] reels;
    private static volatile SlotMachine instance;

    private SlotMachine() {
        reels = new Symbol[MAX_REELS];
    }

    public static SlotMachine getInstance() {
        if (instance == null) {
            synchronized (SlotMachine.class) {
                if (instance == null) {
                    instance = new SlotMachine();
                }
            }
        }
        return instance;
    }

    void startSpin() {
        if (spin()) {
            int bonusSpins = 0;
            while (bonusSpins++ < MAX_BONUS_SPINS && spin());
        }
        System.out.println("----------------------");
    }

    boolean spin() {
        System.out.println("Spinning ............");
        for (int i = 0; i < reels.length; i++) {
            reels[i] = Symbol.getRandomSymbol();
            System.out.print(reels[i] + (i < reels.length - 1 ? " | " : "\n"));
        }
        long winningAmount = calculateWinningAmount();
        if (winningAmount > 0) {
            System.out.println("You have won " + winningAmount);
            return true;
        } else {
            System.out.println("Better Luck Next Time.");
            return false;
        }
    }

    private boolean isJackpot() {
        for (int i = 0; i < reels.length - 1; i++) {
            if (reels[i] != reels[i + 1]) return false;
        }
        return true;
    }

    private long calculateWinningAmount() {
        long winningAmount = 0;
        if (isJackpot()) {
            for (Symbol reel : reels) {
                winningAmount += reel.getMultiplier();
            }
        }
        return winningAmount;
    }
}
