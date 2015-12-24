/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import java.util.Collections;
import java.util.List;

import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class RequiredParameter extends Parameter {

	private static final long serialVersionUID = 2544143727820268303L;

	public RequiredParameter(final SymbolStruct var) {
		this(var, false);
	}

	public RequiredParameter(final SymbolStruct var, final boolean isSpecial) {
		this(var, null, isSpecial);
	}

	public RequiredParameter(final SymbolStruct var, final DestructuringLambdaList destructuringForm) {
		this(var, destructuringForm, false);
	}

	public RequiredParameter(final SymbolStruct var, final DestructuringLambdaList destructuringForm,
	                         final boolean isSpecial) {
		super(var, destructuringForm, TType.INSTANCE, null, isSpecial);
	}

	public static final class Builder {

		private final SymbolStruct var;

		private DestructuringLambdaList destructuringForm;

		private boolean isSpecial;

		public Builder(final PackageStruct aPackage, final String symbolName) {
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

		public RequiredParameter build() {
			return new RequiredParameter(var, destructuringForm, isSpecial);
		}

		public List<RequiredParameter> buildList() {
			return Collections.singletonList(build());
		}
	}
}
