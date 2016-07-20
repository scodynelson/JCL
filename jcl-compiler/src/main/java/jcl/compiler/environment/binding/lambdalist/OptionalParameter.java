/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import java.util.Collections;
import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.list.NILStruct;
import jcl.type.TType;

public class OptionalParameter extends Parameter {

	private final SuppliedPParameter suppliedPBinding;

	public OptionalParameter(final SymbolStruct var, final LispStruct initForm, final SuppliedPParameter suppliedPBinding) {
		this(var, initForm, false, suppliedPBinding);
	}

	public OptionalParameter(final SymbolStruct var, final LispStruct initForm, final boolean isSpecial,
	                         final SuppliedPParameter suppliedPBinding) {
		this(var, null, initForm, isSpecial, suppliedPBinding);
	}

	public OptionalParameter(final SymbolStruct var, final DestructuringLambdaList destructuringForm,
	                         final LispStruct initForm, final SuppliedPParameter suppliedPBinding) {
		this(var, destructuringForm, initForm, false, suppliedPBinding);
	}

	public OptionalParameter(final SymbolStruct var, final DestructuringLambdaList destructuringForm,
	                         final LispStruct initForm, final boolean isSpecial, final SuppliedPParameter suppliedPBinding) {
		super(var, destructuringForm, TType.INSTANCE, initForm, isSpecial);
		this.suppliedPBinding = suppliedPBinding;
	}

	public SuppliedPParameter getSuppliedPBinding() {
		return suppliedPBinding;
	}

	public static Builder builder(final PackageStruct aPackage, final String symbolName) {
		return new Builder(aPackage, symbolName);
	}

	public static final class Builder {

		private final SymbolStruct var;

		private DestructuringLambdaList destructuringForm;

		private LispStruct initForm = NILStruct.INSTANCE;

		private SuppliedPParameter suppliedPBinding;

		private boolean isSpecial;

		private Builder(final PackageStruct aPackage, final String symbolName) {
			var = aPackage.intern(symbolName).getSymbol();
		}

		public Builder destructuringForm(final DestructuringLambdaList destructuringForm) {
			this.destructuringForm = destructuringForm;
			return this;
		}

		public Builder initForm(final LispStruct initForm) {
			this.initForm = initForm;
			return this;
		}

		public Builder isSpecial() {
			isSpecial = true;
			return this;
		}

		public Builder suppliedPBinding() {
			final PackageStruct aPackage = var.getSymbolPackage();
			final String symbolName = var.getName();

			final SymbolStruct suppliedP = aPackage.intern(symbolName + "-P-" + System.nanoTime()).getSymbol();
			return suppliedPBinding(new SuppliedPParameter(suppliedP));
		}

		public Builder suppliedPBinding(final SuppliedPParameter suppliedPBinding) {
			this.suppliedPBinding = suppliedPBinding;
			return this;
		}

		public OptionalParameter build() {
			return new OptionalParameter(var, destructuringForm, initForm, isSpecial, suppliedPBinding);
		}

		public List<OptionalParameter> buildList() {
			return Collections.singletonList(build());
		}
	}
}
