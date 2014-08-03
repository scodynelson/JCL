package jcl.compiler.old;

import jcl.structs.symbols.SymbolStruct;
import jcl.types.Null;

import java.util.Collections;
import java.util.List;

public class ArgHandler {

	Object[] args;
	int minArgs;
	int maxArgs;
	boolean hasRest;

	/**
	 * Creates a new instance of ArgHandler
	 */
	public ArgHandler(List<?> args) {
		this.args = args.toArray();
		checkArgs();
		countArgs();
	}

	public void countArgs() {
		int min = 0;
		int max = 0;
		boolean optional = false;
		boolean rest = false;
		for (int i = 0; i < args.length; i++) {
			if (!optional && !rest) {
				if (((SymbolStruct) args[i]).getName().equals("&OPTIONAL")) {
					optional = true;
				} else if (((SymbolStruct) args[i]).getName().equals("&REST")) {
					rest = true;
				} else {
					min++;
					max++;
				}
			} else if (!rest) {
				if (((SymbolStruct) args[i]).getName().equals("&REST")) {
					rest = true;
				} else {
					max++;
				}
			}
		}
		this.minArgs = min;
		this.maxArgs = max;
		this.hasRest = rest;
	}

	public void checkArgs() {
		for (int i = 0; i < args.length; i++) {
			if (!(args[i] instanceof SymbolStruct)) {
				throw new RuntimeException("All arguments must be of type Symbol.");
			}
		}
	}

	public static Object[] massageArgs(int minArgs, int maxArgs, boolean hasRest, Object[] tstArgs) {
		Object[] returnArgs;
		Object[] restArgs;
		int i;
		int neededArgs = maxArgs;
		int totalArgs = neededArgs;
		int numRestArgs;

		if (hasRest) {
			totalArgs++;
		}

		if (tstArgs.length == totalArgs) {
			returnArgs = tstArgs;
		} else {
			if (tstArgs.length < minArgs) {
				throw new RuntimeException("Not enough arguments.");
			}
			if (tstArgs.length > maxArgs && !hasRest) {
				throw new RuntimeException("Too many arguments.");
			}

			returnArgs = new Object[totalArgs];

			for (i = 0; i < neededArgs; i++) {
				if (i < tstArgs.length) {
					returnArgs[i] = tstArgs[i];
				} else {
					returnArgs[i] = Null.INSTANCE;
				}
			}
			numRestArgs = tstArgs.length - i;
			if (hasRest) {
				if (numRestArgs > 0) {
					restArgs = new Object[numRestArgs];
					for (int j = 0; j < numRestArgs; j++) {
						restArgs[j] = tstArgs[i + j];
					}
				} else {
					restArgs = new Object[]{};
				}
				returnArgs[returnArgs.length - 1] = Collections.singletonList(restArgs);
			}
		}
		return returnArgs;
	}

	public int getMinArgs() {
		return minArgs;
	}

	public int getMaxArgs() {
		return maxArgs;
	}

	public boolean getHasRest() {
		return hasRest;
	}
}
