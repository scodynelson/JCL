/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.declare;

import java.util.ArrayList;
import java.util.List;

import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class DeclareStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -7730761501615283012L;

	private final List<SpecialDeclarationStruct> specialDeclarations = new ArrayList<>();

	private JavaClassNameDeclarationStruct javaClassNameDeclaration;

	public List<SpecialDeclarationStruct> getSpecialDeclarations() {
		return specialDeclarations;
	}

	public JavaClassNameDeclarationStruct getJavaClassNameDeclaration() {
		return javaClassNameDeclaration;
	}

	public void setJavaClassNameDeclaration(final JavaClassNameDeclarationStruct javaClassNameDeclaration) {
		this.javaClassNameDeclaration = javaClassNameDeclaration;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(specialDeclarations)
		                            .append(javaClassNameDeclaration)
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
		final DeclareStruct rhs = (DeclareStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(specialDeclarations, rhs.specialDeclarations)
		                          .append(javaClassNameDeclaration, rhs.javaClassNameDeclaration)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(specialDeclarations)
		                                                                .append(javaClassNameDeclaration)
		                                                                .toString();
	}
}
