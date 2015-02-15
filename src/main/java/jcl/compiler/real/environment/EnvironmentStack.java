/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.util.Stack;

public class EnvironmentStack extends Stack<Environment> {

	private static final long serialVersionUID = 8920610864631178978L;

	public Environment getCurrentLexicalEnvironment() {

		// First lets check the top to see if it's Lexical (most of the time it will be).
		Environment currentEnvironment = peek();
		if (currentEnvironment != null) {
			return currentEnvironment;
		}

		// Temporary Stack to store the current environment as we traverse.
		final Stack<Environment> tempStack = new Stack<>();

		try {
			while (currentEnvironment == null) {
				currentEnvironment = pop();
				tempStack.push(currentEnvironment);
			}
		} finally {
			// Make sure we restore the current stack to its previous state
			while (!tempStack.isEmpty()) {
				push(tempStack.pop());
			}
		}

		return currentEnvironment;
	}
}
