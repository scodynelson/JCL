/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class OptionalParameter extends Parameter {

	private static final long serialVersionUID = 3357381481589151323L;

	private final SuppliedPParameter suppliedPBinding;

	public OptionalParameter(final SymbolStruct<?> var, final LispStruct initForm, final SuppliedPParameter suppliedPBinding) {
		this(var, initForm, false, suppliedPBinding);
	}

	public OptionalParameter(final SymbolStruct<?> var, final LispStruct initForm, final boolean isSpecial,
	                         final SuppliedPParameter suppliedPBinding) {
		this(var, null, initForm, isSpecial, suppliedPBinding);
	}

	public OptionalParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                         final LispStruct initForm, final SuppliedPParameter suppliedPBinding) {
		this(var, destructuringForm, initForm, false, suppliedPBinding);
	}

	public OptionalParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                         final LispStruct initForm, final boolean isSpecial, final SuppliedPParameter suppliedPBinding) {
		super(var, destructuringForm, TType.INSTANCE, initForm, isSpecial);
		this.suppliedPBinding = suppliedPBinding;
	}

	public SuppliedPParameter getSuppliedPBinding() {
		return suppliedPBinding;
	}

	public static final class Builder {

		private final SymbolStruct<?> var;

		private DestructuringLambdaList destructuringForm;

		private LispStruct initForm = NullStruct.INSTANCE;

		private SuppliedPParameter suppliedPBinding;

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

		public Builder suppliedPBinding() {
			final PackageStruct aPackage = var.getSymbolPackage();
			final String symbolName = var.getName();

			final SymbolStruct<?> suppliedP = aPackage.intern(symbolName + "-P-" + System.nanoTime()).getSymbol();
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
