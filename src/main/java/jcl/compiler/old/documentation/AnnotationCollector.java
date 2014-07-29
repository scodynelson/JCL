package jcl.compiler.old.documentation;

import jcl.compiler.old.EmptyVisitor;
import org.objectweb.asm.AnnotationVisitor;

import java.util.Hashtable;

public class AnnotationCollector extends EmptyVisitor {

	private Hashtable<String, Object> fields = new Hashtable<>();
	private boolean include = false;

	public Hashtable<String, Object> getTable() {
		return fields;
	}

	@Override
	public AnnotationVisitor visitAnnotation(String sig, boolean visible) {
		if (sig.equalsIgnoreCase("Llisp/system/documentation/DocStringAnn;")) {
			include = true;
		} else {
			include = false;
		}

		return super.visitAnnotation(sig, visible);
	}
}
