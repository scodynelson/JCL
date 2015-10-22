/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LetStruct extends ClosureCreationStruct<LetStruct.LetVar> {

	private static final long serialVersionUID = -3186671381163635893L;

	public LetStruct(final List<LetVar> vars, final PrognStruct forms, final Environment letEnvironment) {
		super(vars, forms, letEnvironment);
	}

	public static class LetVar implements Serializable {

		private static final long serialVersionUID = 3246152127057600416L;

		private final SymbolStruct<?> var;

		private final LispStruct initForm;

		private final boolean isSpecial;

		public LetVar(final SymbolStruct<?> var, final LispStruct initForm, final boolean isSpecial) {
			this.var = var;
			this.initForm = initForm;
			this.isSpecial = isSpecial;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public LispStruct getInitForm() {
			return initForm;
		}

		public boolean isSpecial() {
			return isSpecial;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().append(var)
			                            .append(initForm)
			                            .toHashCode();
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj == null) {
				return false;
			}
			if (obj == this) {
				return true;
			}
			if (obj.getClass() != getClass()) {
				return false;
			}
			final LetVar rhs = (LetVar) obj;
			return new EqualsBuilder().append(var, rhs.var)
			                          .append(initForm, rhs.initForm)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(var)
			                                                                .append(initForm)
			                                                                .toString();
		}
	}
}
