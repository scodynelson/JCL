/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class BlockStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -115779602179582479L;

	private final SymbolStruct<?> name;

	private final PrognStruct forms;

	public BlockStruct(final SymbolStruct<?> name, final PrognStruct forms) {
		this.name = name;
		this.forms = forms;
	}

	public SymbolStruct<?> getName() {
		return name;
	}

	public PrognStruct getForms() {
		return forms;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(name)
		                            .append(forms)
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
		final BlockStruct rhs = (BlockStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(name, rhs.name)
		                          .append(forms, rhs.forms)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(name)
		                                                                .append(forms)
		                                                                .toString();
	}
}
