/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/giu/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.pdf;

import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.ENCRYPTED;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.LOADED;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.LOADED_WITH_USER_PWD_DECRYPTION;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.LOADING;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.WITH_ERRORS;
import static org.sejda.impl.itext5.util.ITextUtils.nullSafeClosePdfReader;

import java.util.Collection;

import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.sejda.conversion.PdfVersionAdapter;
import org.sejda.impl.itext5.component.DefaultPdfSourceOpener;
import org.sejda.model.exception.TaskWrongPasswordException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.itextpdf.text.pdf.PdfReader;

/**
 * iText implementation of the load service
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class ITextPdfLoadService implements PdfLoadService {

    private static final Logger LOG = LoggerFactory.getLogger(ITextPdfLoadService.class);

    public void load(Collection<PdfDocumentDescriptor> toLoad) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Loading"));
        for (PdfDocumentDescriptor current : toLoad) {
            if (!current.isInvalid()) {
                LOG.trace("Loading {}", current.getFileName());
                PdfReader reader = null;
                try {
                    current.moveStatusTo(LOADING);
                    reader = current.toPdfFileSource().open(new DefaultPdfSourceOpener());
                    current.setPages(reader.getNumberOfPages());
                    current.setVersion(new PdfVersionAdapter(Character.toString(reader.getPdfVersion())).getEnumValue());
                    current.setInformationDictionary(reader.getInfo());
                    if (current.hasPassword()) {
                        current.moveStatusTo(LOADED_WITH_USER_PWD_DECRYPTION);
                    } else {
                        current.moveStatusTo(LOADED);
                    }
                } catch (TaskWrongPasswordException twpe) {
                    current.moveStatusTo(ENCRYPTED);
                    LOG.warn("User password required for '{}'", current.getFileName(), twpe);
                } catch (Exception e) {
                    LOG.error("An error occured loading the document '{}'", current.getFileName(), e);
                    current.moveStatusTo(WITH_ERRORS);
                } finally {
                    try {
                        nullSafeClosePdfReader(reader);
                    } catch (RuntimeException e) {
                        LOG.warn("An error occured closing the document", e);
                    }
                }
            } else {
                LOG.trace("Skipping invalid document {}", current.getFileName());
            }
        }
        LOG.debug(DefaultI18nContext.getInstance().i18n("Documents loaded"));
    }

}
