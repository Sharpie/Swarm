/*
 * @(#)IndexBuilder.java	1.12 98/09/22
 *
 * Copyright 1998 by Sun Microsystems, Inc.,
 * 901 San Antonio Road, Palo Alto, California, 94303, U.S.A.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information
 * of Sun Microsystems, Inc. ("Confidential Information").  You
 * shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement
 * you entered into with Sun.
 */

//package com.sun.tools.doclets;

import com.sun.tools.doclets.*;
import com.sun.javadoc.*;
import java.io.*;
import java.lang.*;
import java.util.*;

/**
 * Build the mapping of each Unicode character with it's member lists
 * containing members names starting with it. Also build a list for all the
 * Unicode characters which start a member name. Member name is
 * classkind or field or method or constructor name.
 *
 * @since JDK1.2
 * @see java.lang.Character
 * @author Atul M Dambalkar
 */
public class IndexBuilder {

    /**
     * Mapping of each Unicode Character with the member list containing
     * members with names starting with it.
     */
    private Map indexmap = new HashMap();

    /**
     * Don't generate deprecated information if true.
     */
    private boolean noDeprecated;

    /**
     * Build this Index only for classes?
     */
    private boolean classesOnly;

    // make ProgramElementDoc[] when new toArray is available
    protected final Object[] elements;  

    /**
     * Constructor. Build the index map.
     *
     * @param root Root of the Tree.
     * @param noDeprecated true if -nodeprecated option is used,
     * false otherwise.
     */
    public IndexBuilder(RootDoc root, boolean noDeprecated) {
        this(root, noDeprecated, false);
    }

    /**
     * Constructor. Build the index map.
     *
     * @param root Root of the Tree.
     * @param noDeprecated true if -nodeprecated option is used,
     * false otherwise.
     * @param classesOnly Include only classes in index.
     */
    public IndexBuilder(RootDoc root, boolean noDeprecated, 
                        boolean classesOnly) {
        if (classesOnly) {
            Configuration.message.
                              notice("doclet.Building_Index_For_All_Classes");
        } else {
            Configuration.message.notice("doclet.Building_Index");
        }
        this.noDeprecated = noDeprecated;
        this.classesOnly = classesOnly;
        buildIndexMap(root);
        Set set = indexmap.keySet();
        elements =  set.toArray();
        Arrays.sort(elements);
    }

    /**
     * Sort the index map. Traverse the index map for all it's elements and
     * sort each element which is a list.
     */
    protected void sortIndexMap() {
        for (Iterator it = indexmap.values().iterator(); it.hasNext(); ) {
            Collections.sort((List)it.next());
        } 
    }

    /**
     * Get all the members in all the Packages and all the Classes 
     * given on the command line. Form separate list of those members depending
     * upon their names.
     *
     * @param root Root of the documemt.
     */
    protected void buildIndexMap(RootDoc root)  {
        PackageDoc[] packages = root.specifiedPackages();
        ClassDoc[] classes = root.classes();
        List list = new ArrayList();
        if (!classesOnly) {
            adjustIndexMap(packages);
        }
        adjustIndexMap(classes);
        if (!classesOnly) {
            for (int i = 0; i < classes.length; i++) {
                if (shouldAddToIndexMap(classes[i])) {
                    putMembersInIndexMap(classes[i]);
                }
            }
        }
        sortIndexMap();
    }

    /**
     * Put all the members(fields, methods and constructors) in the classdoc
     * to the indexmap. 
     *
     * @param classdoc ClassDoc whose members will be added to the indexmap.
     */
    protected void putMembersInIndexMap(ClassDoc classdoc) {
        adjustIndexMap(classdoc.fields());
        adjustIndexMap(classdoc.methods());
        adjustIndexMap(classdoc.constructors());
    }
        

    /**
     * Adjust list of members according to their names. Check the first
     * character in a member name, and then add the member to a list of members
     * for that particular unicode character.
     * 
     * @param elements Array of members.
     */
    protected void adjustIndexMap(Doc[] elements) {
        for (int i = 0; i < elements.length; i++) {
            if (shouldAddToIndexMap(elements[i])) {
                String name = elements[i].name();
                char ch = (name.length()==0)? 
                    '*' : 
                    Character.toUpperCase(name.charAt(0));
                Character unicode = new Character(ch);
                List list = (List)indexmap.get(unicode); 
                if (list == null) {
                    list = new ArrayList();
                    indexmap.put(unicode, list); 
                }     
                list.add(elements[i]);
            }
        }
    }
    
    /**
     * Should this doc element be added to the index map?
     */ 
    protected boolean shouldAddToIndexMap(Doc element) {
        boolean ret_val = 
            !(noDeprecated && element.tags("deprecated").length > 0) &&
            !(Standard.configuration().hide && 
              element.tags("hide").length > 0);

        return ret_val;
        
    }
 
    /**
     * Return a map of all the individual member lists with Unicode character.
     *
     * @return Map index map.
     */ 
    public Map getIndexMap() {
        return indexmap;
    }

    /**
     * Return the sorted list of members, for passed Unicode Character. 
     *
     * @param index index Unicode character.
     * @return List member list for specific Unicode character.
     */
    public List getMemberList(Character index) {
        return (List)indexmap.get(index);
    }

    /**
     * Array of IndexMap keys, Unicode characters.
     */
    public Object[] elements() {
        return elements;
    }
}
