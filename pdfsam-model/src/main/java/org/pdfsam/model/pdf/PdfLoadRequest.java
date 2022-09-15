/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/giu/2013
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.model.pdf;

import org.pdfsam.model.tool.BaseToolBound;

import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * Request the app to load one or many pdf documents
 *
 * @author Andrea Vacondio
 */
public class PdfLoadRequest extends BaseToolBound {
    private Collection<PdfDocumentDescriptor> documents = new ConcurrentLinkedQueue<>();

    public PdfLoadRequest(String toolId) {
        super(toolId);
    }

    public boolean add(PdfDocumentDescriptor e) {
        return documents.add(e);
    }

    public boolean addAll(Collection<PdfDocumentDescriptor> c) {
        return documents.addAll(c);
    }

    public Collection<PdfDocumentDescriptor> getDocuments() {
        return Collections.unmodifiableCollection(documents);
    }
}
