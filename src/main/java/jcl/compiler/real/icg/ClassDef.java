/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;

/**
* Created by codynelson on 3/10/15.
*/
public class ClassDef {
	private final ClassWriter cw;
	private MethodVisitor mv;
	private FieldVisitor fv;
	private AnnotationVisitor av;

	private final String name;

	ClassDef(final ClassWriter cw, final String name) {
		this.cw = cw;
		this.name = name;
	}

	public ClassWriter getCw() {
		return cw;
	}

	public MethodVisitor getMv() {
		return mv;
	}

	public void setMv(final MethodVisitor mv) {
		this.mv = mv;
	}

	public FieldVisitor getFv() {
		return fv;
	}

	public void setFv(final FieldVisitor fv) {
		this.fv = fv;
	}

	public AnnotationVisitor getAv() {
		return av;
	}

	public void setAv(final AnnotationVisitor av) {
		this.av = av;
	}

	public String getName() {
		return name;
	}
}
