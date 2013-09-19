/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/giu/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.pdf;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.input.PdfSource;
import org.sejda.model.pdf.PdfMetadataKey;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * Lightweight pdf document descriptor holding data necessary to fill the selection table and request a task execution.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class PdfDocumentDescriptor {

    private UUID uuid;
    private int pages;
    private EncryptionStatus encryptionStatus;
    private String password;
    private File file;
    private String version;
    private Map<PdfMetadataKey, String> metadata = new HashMap<>();

    private PdfDocumentDescriptor(File file, String password) {
        this.file = file;
        this.password = password;
        this.uuid = UUID.randomUUID();
    }

    public UUID getUuid() {
        return uuid;
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

    public EncryptionStatus getEncryptionStatus() {
        return encryptionStatus;
    }

    public void setEncryptionStatus(EncryptionStatus encryptionStatus) {
        this.encryptionStatus = encryptionStatus;
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

    public static PdfDocumentDescriptor newDescriptor(File file, String password) {
        requireNotNull(file, "Input file is mandatory");
        return new PdfDocumentDescriptor(file, password);
    }

    public static PdfDocumentDescriptor newDescriptorNoPassword(File file) {
        requireNotNull(file, "Input file is mandatory");
        return new PdfDocumentDescriptor(file, null);
    }

    public static PdfDocumentDescriptor newCopy(PdfDocumentDescriptor toCopy) {
        requireNotNull(toCopy, "Cannot copy a null document descriptor");
        PdfDocumentDescriptor copy = new PdfDocumentDescriptor(toCopy.file, toCopy.password);
        copy.uuid = toCopy.uuid;
        copy.encryptionStatus = toCopy.getEncryptionStatus();
        copy.pages = toCopy.getPages();
        copy.version = toCopy.getVersion();
        for (Entry<PdfMetadataKey, String> entry : toCopy.metadata.entrySet()) {
            copy.addMedatada(entry.getKey(), entry.getValue());
        }
        return copy;
    }
}
