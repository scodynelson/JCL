/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.lang.PackageStruct;
import jcl.lang.SymbolStructImpl;
import jcl.type.ListType;

public class RestParameter extends Parameter {

	public RestParameter(final SymbolStructImpl var) {
		this(var, false);
	}

	public RestParameter(final SymbolStructImpl var, final boolean isSpecial) {
		this(var, null, isSpecial);
	}

	public RestParameter(final SymbolStructImpl var, final DestructuringLambdaList destructuringForm) {
		this(var, destructuringForm, false);
	}

	public RestParameter(final SymbolStructImpl var, final DestructuringLambdaList destructuringForm,
	                     final boolean isSpecial) {
		super(var, destructuringForm, ListType.INSTANCE, null, isSpecial);
	}

	public static Builder builder(final PackageStruct aPackage, final String symbolName) {
		return new Builder(aPackage, symbolName);
	}

	public static final class Builder {

		private final SymbolStructImpl var;

		private DestructuringLambdaList destructuringForm;

		private boolean isSpecial;

		private Builder(final PackageStruct aPackage, final String symbolName) {
			var = aPackage.intern(symbolName).getSymbol();
		}

		public Builder destructuringForm(final DestructuringLambdaList destructuringForm) {
			this.destructuringForm = destructuringForm;
			return this;
		}

		public Builder isSpecial() {
			isSpecial = true;
			return this;
		}

		public RestParameter build() {
			return new RestParameter(var, destructuringForm, isSpecial);
		}
	}
}
