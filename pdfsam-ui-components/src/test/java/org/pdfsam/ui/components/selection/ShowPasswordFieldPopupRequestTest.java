/*
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ott/2014
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
package org.pdfsam.ui.components.selection;

import javafx.scene.layout.Region;
import org.junit.jupiter.api.Test;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

/**
 * @author Andrea Vacondio
 */
@SuppressWarnings("unused")
public class ShowPasswordFieldPopupRequestTest {

    @Test
    public void nullDescriptor() {
        assertThrows(IllegalArgumentException.class, () -> new ShowPasswordFieldPopupRequest(null, new Region()));
    }

    @Test
    public void nullRegion() {
        assertThrows(IllegalArgumentException.class,
                () -> new ShowPasswordFieldPopupRequest(mock(PdfDocumentDescriptor.class), null));
    }

    @Test
    public void nullBoth() {
        assertThrows(IllegalArgumentException.class, () -> new ShowPasswordFieldPopupRequest(null, null));
    }
}
