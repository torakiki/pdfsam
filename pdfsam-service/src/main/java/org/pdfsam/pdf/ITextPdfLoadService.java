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

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.sejda.impl.itext5.util.ITextUtils.nullSafeClosePdfReader;

import java.util.Collection;
import java.util.Map;

import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.sejda.impl.itext5.component.DefaultPdfSourceOpener;
import org.sejda.model.exception.TaskWrongPasswordException;
import org.sejda.model.pdf.PdfMetadataKey;
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
                PdfReader reader = null;
                try {
                    reader = current.toPdfFileSource().open(new DefaultPdfSourceOpener());
                    if (current.encryptionStatusProperty().get() == EncryptionStatus.DECRYPTION_REQUESTED) {
                        current.setEncryptionStatus(EncryptionStatus.DECRYPTED_WITH_USER_PWD);
                    } else {
                        current.setEncryptionStatus(EncryptionStatus.NOT_ENCRYPTED);
                    }
                    current.setPages(reader.getNumberOfPages());
                    current.setVersion(String.format("1.%c", reader.getPdfVersion()));
                    Map<String, String> meta = reader.getInfo();
                    for (PdfMetadataKey key : PdfMetadataKey.values()) {
                        current.addMedatada(key, defaultString(meta.get(key.getKey())));
                    }
                } catch (TaskWrongPasswordException twpe) {
                    current.setEncryptionStatus(EncryptionStatus.ENCRYPTED);
                    LOG.warn(String.format("User password required %s", current.getFileName()), twpe);
                } catch (Exception e) {
                    LOG.error(String.format("An error occured loading the document %s", current.getFileName()), e);
                } finally {
                    nullSafeClosePdfReader(reader);
                }
                current.loaded();
            } else {
                LOG.trace("Skipping cancelled document {}", current.getFileName());
            }
        }
        LOG.debug(DefaultI18nContext.getInstance().i18n("Documents loaded"));
    }

}
