/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mar/2015
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
package org.pdfsam.pdfbox.component;

import static java.util.Objects.requireNonNull;
import static org.sejda.model.rotation.Rotation.getRotation;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.sejda.model.rotation.PageRotation;
import org.sejda.model.rotation.RotationType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
/**
 * Component performing pages rotation
 * 
 * @author Andrea Vacondio
 *
 */
public class PdfRotator implements OngoingRotation {

    private static final Logger LOG = LoggerFactory.getLogger(PdfRotator.class);

    private PDDocument document;
    private PageRotation rotation;

    private PdfRotator(PageRotation rotation) {
        requireNonNull(rotation, "Rotation cannot be null");
        this.rotation = rotation;
    }

    /**
     * DSL entry point to apply a rotation
     * <p>
     * <code>applyRotation(rotation).to(document);</code>
     * </p>
     * 
     * @param rotation
     * @return the ongoing apply rotation exposing methods to set the document you want to apply the rotation to.
     */
    public static OngoingRotation applyRotation(PageRotation rotation) {
        return new PdfRotator(rotation);
    }

    public void to(PDDocument document) {
        this.document = document;
        executeRotation();
    }

    /**
     * Apply the rotation to the dictionary of the pages specified in the {@link PageRotation}
     */
    private void executeRotation() {
        RotationType type = rotation.getRotationType();
        LOG.debug("Applying rotation of {} to pages {}", rotation.getRotation().getDegrees(), type);
        if (RotationType.SINGLE_PAGE.equals(type)) {
            apply(rotation.getPageNumber());
        } else {
            for (int j = 1; j <= document.getNumberOfPages(); j++) {
                apply(j);
            }
        }
    }

    /**
     * apply the rotation to the given page if necessary
     * 
     * @param pageNmber
     */
    private void apply(int pageNmber) {
        if (rotation.accept(pageNmber)) {
            PDPage page = document.getPage(pageNmber - 1);
            page.setRotation(rotation.getRotation().addRotation(getRotation(page.getRotation())).getDegrees());
        }
    }
}
