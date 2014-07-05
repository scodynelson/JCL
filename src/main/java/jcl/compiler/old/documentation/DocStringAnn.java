package jcl.compiler.old.documentation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

//TYPE is used for Class declaration
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME) 

public @interface DocStringAnn {
    String docString();
    String docUID();
    String javaClassName();
    String generated();
    String javaName();
}