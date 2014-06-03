/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/giu/2014
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
package org.pdfsam.split;

import static org.pdfsam.support.RequireUtils.requireNotNull;
import javafx.scene.control.RadioButton;

import org.sejda.model.pdf.page.PredefinedSetOfPages;

/**
 * A radio button associated to a Predefined set of pages
 * 
 * @author Andrea Vacondio
 *
 */
class PredefinedSetOfPagesRadioButton extends RadioButton {

    private PredefinedSetOfPages pages;

    public PredefinedSetOfPagesRadioButton(PredefinedSetOfPages pages, String text) {
        super(text);
        requireNotNull(pages, "Cannot create the radio button with a null predefined set of pages.");
        this.pages = pages;
    }

    public PredefinedSetOfPages getPages() {
        return pages;
    }

}
