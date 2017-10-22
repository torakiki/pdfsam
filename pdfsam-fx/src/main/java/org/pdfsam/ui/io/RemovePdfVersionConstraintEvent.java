/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/feb/2013
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
package org.pdfsam.ui.io;

import static org.pdfsam.support.RequireUtils.requireNotNull;

import org.sejda.model.pdf.PdfVersion;

/**
 * Event to notify subscribers that some selected option removes a constraint on the output pdf version.
 * 
 * @author Andrea Vacondio
 * 
 */
class RemovePdfVersionConstraintEvent extends BasePdfVersionEvent {

    public RemovePdfVersionConstraintEvent(PdfVersion pdfVersion) {
        super(pdfVersion);
        requireNotNull(pdfVersion, "Unable to create a pdf version event on a null pdf version");
    }
}
