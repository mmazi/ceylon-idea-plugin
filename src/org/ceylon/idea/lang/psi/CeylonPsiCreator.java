/*
 * Copyright 2000-2009 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.ceylon.idea.lang.psi;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import org.ceylon.idea.lang.parser.CeylonElementTypes;

public class CeylonPsiCreator {

    /**
     * Creates Ceylon PSI element by given AST node
     *
     * @param node Given node
     * @return Respective PSI element
     */
    public static PsiElement createElement(ASTNode node) {
        IElementType elem = node.getElementType();

        if (elem == CeylonElementTypes.IMPORT_LIST) return new CeylonImportList(node);
        if (elem == CeylonElementTypes.IMPORT) return new CeylonImport(node);
        if (elem == CeylonElementTypes.L_IDENTIFIER) return new CeylonLIdentifier(node);

        return new ASTWrapperPsiElement(node);
    }

}
