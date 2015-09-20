/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.selection;

import static org.mockito.Mockito.mock;

import org.junit.Test;
import org.pdfsam.pdf.PdfDocumentDescriptor;

import javafx.scene.layout.Region;

/**
 * @author Andrea Vacondio
 *
 */
@SuppressWarnings("unused")
public class ShowPasswordFieldPopupRequestTest {

    @Test(expected = IllegalArgumentException.class)
    public void nullDescriptor() {
        new ShowPasswordFieldPopupRequest(null, new Region());
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullRegion() {
        new ShowPasswordFieldPopupRequest(mock(PdfDocumentDescriptor.class), null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullBoth() {
        new ShowPasswordFieldPopupRequest(null, null);
    }
}
