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

import static org.pdfsam.support.RequireUtils.requireNotNull;

import java.io.File;
import java.util.HashMap;
import java.util.concurrent.atomic.AtomicBoolean;

import javafx.beans.property.ReadOnlyIntegerProperty;
import javafx.beans.property.ReadOnlyIntegerWrapper;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;

import org.apache.commons.lang3.StringUtils;
import org.sejda.model.input.PdfFileSource;

/**
 * Lightweight pdf document descriptor holding data necessary to fill the selection table and request a task execution.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class PdfDocumentDescriptor {

    private ReadOnlyObjectWrapper<LoadingStatus> loadingStatus = new ReadOnlyObjectWrapper<>(LoadingStatus.REQUESTED);
    private AtomicBoolean invalid = new AtomicBoolean(false);
    private ReadOnlyIntegerWrapper pages = new ReadOnlyIntegerWrapper(0);
    private ReadOnlyObjectWrapper<EncryptionStatus> encryptionStatus = new ReadOnlyObjectWrapper<>(
            EncryptionStatus.NOT_ENCRYPTED);
    private String password;
    private File file;
    private String version;
    private HashMap<String, String> metadata = new HashMap<>();

    private PdfDocumentDescriptor(File file, String password) {
        this.file = file;
        this.password = password;
    }

    public String getFileName() {
        return file.getName();
    }

    public PdfFileSource toPdfFileSource() {
        return PdfFileSource.newInstanceWithPassword(file, password);
    }

    /**
     * @param key
     * @return the information dictionary value for the key or an empty string
     */
    public String getInformation(String key) {
        return StringUtils.defaultString(metadata.get(key));
    }

    public void setInformationDictionary(HashMap<String, String> info) {
        metadata.clear();
        metadata.putAll(info);
    }

    public ReadOnlyIntegerProperty pagesPropery() {
        return pages.getReadOnlyProperty();
    }

    public void setPages(int pages) {
        this.pages.set(pages);
    }

    public ReadOnlyObjectProperty<LoadingStatus> loadedProperty() {
        return loadingStatus.getReadOnlyProperty();
    }

    /**
     * Puts this descriptor in loading state
     */
    public void loading() {
        loadingStatus.set(loadingStatus.get().loading());
    }

    public void loaded() {
        loadingStatus.set(LoadingStatus.LOADED);
    }

    public void loadedWithErrors() {
        loadingStatus.set(LoadingStatus.LOADED_WITH_ERRORS);
    }

    public ReadOnlyObjectProperty<EncryptionStatus> encryptionStatusProperty() {
        return encryptionStatus.getReadOnlyProperty();
    }

    public void setEncryptionStatus(EncryptionStatus encryptionStatus) {
        this.encryptionStatus.set(encryptionStatus);
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

    /**
     * @return true if this descriptor has been invalidated, this can happen if the user deletes it from the UI and it tells to any service performing or about to perform some
     *         action on the descriptor that it should be ignored since not relevant anymore.
     */
    public boolean isInvalid() {
        return invalid.get();
    }

    public void invalidate() {
        this.invalid.set(true);
    }

    public static PdfDocumentDescriptor newDescriptor(File file, String password) {
        requireNotNull(file, "Input file is mandatory");
        return new PdfDocumentDescriptor(file, password);
    }

    public static PdfDocumentDescriptor newDescriptorNoPassword(File file) {
        requireNotNull(file, "Input file is mandatory");
        return new PdfDocumentDescriptor(file, null);
    }

}
