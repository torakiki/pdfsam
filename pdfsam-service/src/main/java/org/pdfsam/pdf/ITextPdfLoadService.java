/*
 * Created on 13/giu/2013
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.pdf;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.sejda.impl.itext.component.input.PdfSourceOpeners;
import org.sejda.model.pdf.PdfMetadataKey;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.lowagie.text.pdf.PdfReader;

import static org.apache.commons.lang3.StringUtils.defaultString;

import static org.pdfsam.pdf.PdfDocumentDescriptor.newCopy;
import static org.sejda.impl.itext.util.ITextUtils.nullSafeClosePdfReader;

/**
 * iText implementation of the load service
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class ITextPdfLoadService implements PdfLoadService {

    private static final Logger LOG = LoggerFactory.getLogger(ITextPdfLoadService.class);

    public List<PdfDocumentDescriptor> load(List<PdfDocumentDescriptor> toLoad) {
        LOG.debug(DefaultI18nContext.getInstance().i18n("Loading documents"));
        List<PdfDocumentDescriptor> loaded = new ArrayList<PdfDocumentDescriptor>(toLoad.size());
        for (PdfDocumentDescriptor current : toLoad) {
            PdfReader reader = null;
            try {
                reader = current.toPdfSource().open(PdfSourceOpeners.newPartialReadOpener());
                PdfDocumentDescriptor copy = newCopy(current);
                copy.setEncrypted(reader.isEncrypted());
                copy.setPages(reader.getNumberOfPages());
                copy.setVersion(String.format("1.%c", reader.getPdfVersion()));
                @SuppressWarnings("unchecked")
                Map<String, String> meta = reader.getInfo();
                for (PdfMetadataKey key : PdfMetadataKey.values()) {
                    copy.addMedatada(key, defaultString(meta.get(key.getKey())));
                }
                loaded.add(copy);
            } catch (Exception e) {
                LOG.error(String.format("An error occured loading the document %s", current.getFileName()), e);
            } finally {
                nullSafeClosePdfReader(reader);
            }
        }
        LOG.debug(DefaultI18nContext.getInstance().i18n("Documents loaded"));
        return loaded;
    }

}
