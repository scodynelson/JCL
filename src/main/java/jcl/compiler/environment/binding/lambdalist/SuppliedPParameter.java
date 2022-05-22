/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;

public class SuppliedPParameter extends Parameter {

	public SuppliedPParameter(final SymbolStruct var) {
		this(var, false);
	}

	public SuppliedPParameter(final SymbolStruct var, final boolean isSpecial) {
		super(var, null, null, isSpecial);
	}

	public static Builder builder(final PackageStruct aPackage, final String symbolName) {
		return new Builder(aPackage, symbolName);
	}

	public static final class Builder {

		private final SymbolStruct var;

		private DestructuringLambdaList destructuringForm;

		private boolean isSpecial;

		private Builder(final PackageStruct aPackage, final String symbolName) {
			var = aPackage.intern(symbolName).getSymbol();
		}

		public Builder isSpecial() {
			isSpecial = true;
			return this;
		}

		public SuppliedPParameter build() {
			return new SuppliedPParameter(var, isSpecial);
		}
	}
}
