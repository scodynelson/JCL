/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import java.util.Collections;
import java.util.List;

import jcl.lang.GlobalPackageStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.SymbolStruct;
import jcl.type.TType;

public class KeyParameter extends Parameter {

	private final SymbolStruct keyName;

	private final SuppliedPParameter suppliedPBinding;

	public KeyParameter(final SymbolStruct var, final LispStruct initForm, final SymbolStruct keyName,
	                    final SuppliedPParameter suppliedPBinding) {
		this(var, initForm, false, keyName, suppliedPBinding);
	}

	public KeyParameter(final SymbolStruct var, final LispStruct initForm, final boolean isSpecial,
	                    final SymbolStruct keyName, final SuppliedPParameter suppliedPBinding) {
		this(var, null, initForm, isSpecial, keyName, suppliedPBinding);
	}

	public KeyParameter(final SymbolStruct var, final DestructuringLambdaList destructuringForm,
	                    final LispStruct initForm, final SymbolStruct keyName, final SuppliedPParameter suppliedPBinding) {
		this(var, destructuringForm, initForm, false, keyName, suppliedPBinding);
	}

	public KeyParameter(final SymbolStruct var, final DestructuringLambdaList destructuringForm,
	                    final LispStruct initForm, final boolean isSpecial, final SymbolStruct keyName,
	                    final SuppliedPParameter suppliedPBinding) {
		super(var, destructuringForm, TType.INSTANCE, initForm, isSpecial);
		this.keyName = keyName;
		this.suppliedPBinding = suppliedPBinding;
	}

	public SymbolStruct getKeyName() {
		return keyName;
	}

	public SuppliedPParameter getSuppliedPBinding() {
		return suppliedPBinding;
	}

	public static Builder builder(final PackageStruct aPackage, final String symbolName) {
		return new Builder(aPackage, symbolName);
	}

	public static Builder builder(final PackageStruct aPackage, final String symbolName, final SymbolStruct keyName) {
		return new Builder(aPackage, symbolName, keyName);
	}

	public static final class Builder {

		private final SymbolStruct var;

		private DestructuringLambdaList destructuringForm;

		private LispStruct initForm = NILStruct.INSTANCE;

		private final SymbolStruct keyName;

		private SuppliedPParameter suppliedPBinding;

		private boolean isSpecial;

		private Builder(final PackageStruct aPackage, final String symbolName) {
			var = aPackage.intern(symbolName).getSymbol();

			final PackageSymbolStruct symbol = GlobalPackageStruct.KEYWORD.findSymbol(symbolName);
			if (symbol == null) {
				keyName = new KeywordStruct(symbolName);
			} else {
				keyName = symbol.getSymbol();
			}
		}

		private Builder(final PackageStruct aPackage, final String symbolName, final SymbolStruct keyName) {
			var = aPackage.intern(symbolName).getSymbol();
			this.keyName = keyName;
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

		public KeyParameter build() {
			return new KeyParameter(var, destructuringForm, initForm, isSpecial, keyName, suppliedPBinding);
		}

		public List<KeyParameter> buildList() {
			return Collections.singletonList(build());
		}
	}
}
