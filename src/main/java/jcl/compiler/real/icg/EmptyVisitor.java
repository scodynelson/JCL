/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.TypePath;

/**
 * An empty implementation of the ASM visitor interfaces.
 */
class EmptyVisitor extends ClassVisitor {

	private final AnnotationVisitor av = new MyAnnotationVisitor();

	public EmptyVisitor() {
		super(Opcodes.ASM5);
	}

	@Override
	public AnnotationVisitor visitAnnotation(final String desc, final boolean visible) {
		return av;
	}

	@Override
	public AnnotationVisitor visitTypeAnnotation(final int typeRef,
	                                             final TypePath typePath, final String desc, final boolean visible) {
		return av;
	}

	@Override
	public FieldVisitor visitField(final int access, final String name, final String desc,
	                               final String signature, final Object value) {
		return new FieldVisitor(Opcodes.ASM5) {

			@Override
			public AnnotationVisitor visitAnnotation(final String desc,
			                                         final boolean visible) {
				return av;
			}

			@Override
			public AnnotationVisitor visitTypeAnnotation(final int typeRef,
			                                             final TypePath typePath, final String desc, final boolean visible) {
				return av;
			}
		};
	}

	@Override
	public MethodVisitor visitMethod(final int access, final String name, final String desc,
	                                 final String signature, final String[] exceptions) {
		return new MethodVisitor(Opcodes.ASM5) {

			@Override
			public AnnotationVisitor visitAnnotationDefault() {
				return av;
			}

			@Override
			public AnnotationVisitor visitAnnotation(final String desc,
			                                         final boolean visible) {
				return av;
			}

			@Override
			public AnnotationVisitor visitTypeAnnotation(final int typeRef,
			                                             final TypePath typePath, final String desc, final boolean visible) {
				return av;
			}

			@Override
			public AnnotationVisitor visitParameterAnnotation(
					final int parameter, final String desc, final boolean visible) {
				return av;
			}

			@Override
			public AnnotationVisitor visitInsnAnnotation(final int typeRef,
			                                             final TypePath typePath, final String desc, final boolean visible) {
				return av;
			}

			@Override
			public AnnotationVisitor visitTryCatchAnnotation(final int typeRef,
			                                                 final TypePath typePath, final String desc, final boolean visible) {
				return av;
			}

			@Override
			public AnnotationVisitor visitLocalVariableAnnotation(
					final int typeRef, final TypePath typePath, final Label[] start,
					final Label[] end, final int[] index, final String desc, final boolean visible) {
				return av;
			}
		};
	}

	private static class MyAnnotationVisitor extends AnnotationVisitor {

		public MyAnnotationVisitor() {
			super(Opcodes.ASM5);
		}

		@Override
		public AnnotationVisitor visitAnnotation(String name, String desc) {
			return this;
		}

		@Override
		public AnnotationVisitor visitArray(String name) {
			return this;
		}
	}
}