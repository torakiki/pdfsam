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

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.support.RequireUtils;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.input.PdfSource;
import org.sejda.model.pdf.PdfMetadataKey;

import static org.pdfsam.support.RequireUtils.require;

/**
 * Lightweight pdf document descriptor holding data necessary to fill the selection table and request a task execution.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class PdfDocumentDescriptor {

    private int pages;
    private boolean encrypted;
    private String password;
    private File file;
    private String version;
    private Map<PdfMetadataKey, String> metadata = new HashMap<PdfMetadataKey, String>();

    private PdfDocumentDescriptor(File file, String password) {
        this.file = file;
        this.password = password;
    }

    public void addMedatada(PdfMetadataKey key, String metadata) {
        this.metadata.put(key, metadata);
    }

    public String getFileName() {
        return file.getName();
    }

    /**
     * @param key
     * @return the metadata value for the key or an empty string
     */
    public String getMedatada(PdfMetadataKey key) {
        return StringUtils.defaultString(metadata.get(key));
    }

    public int getPages() {
        return pages;
    }

    public void setPages(int pages) {
        this.pages = pages;
    }

    public boolean isEncrypted() {
        return encrypted;
    }

    public void setEncrypted(boolean encrypted) {
        this.encrypted = encrypted;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public File getFile() {
        return file;
    }

    public PdfSource<File> toPdfSource() {
        return PdfFileSource.newInstanceWithPassword(file, password);
    }

    public Date getDocumentLastModificationDate() {
        return new Date(file.lastModified());
    }

    public static PdfDocumentDescriptor newDescriptor(File file, String password) {
        require(file != null, "Input file is mandatory");
        return new PdfDocumentDescriptor(file, password);
    }

    public static PdfDocumentDescriptor newDescriptorNoPassword(File file) {
        RequireUtils.require(file != null, "Input file is mandatory");
        return new PdfDocumentDescriptor(file, null);
    }

    public static PdfDocumentDescriptor newCopy(PdfDocumentDescriptor toCopy) {
        require(toCopy != null, "Cannot copy a null document descriptor");
        PdfDocumentDescriptor copy = new PdfDocumentDescriptor(toCopy.file, toCopy.password);
        copy.encrypted = toCopy.isEncrypted();
        copy.pages = toCopy.getPages();
        copy.version = toCopy.getVersion();
        for (Entry<PdfMetadataKey, String> entry : toCopy.metadata.entrySet()) {
            copy.addMedatada(entry.getKey(), entry.getValue());
        }
        return copy;
    }
}
