/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/giu/2013
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
package org.pdfsam.pdf;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.ObservableAtomicReference;
import org.sejda.conversion.exception.ConversionException;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.pdf.PdfVersion;

/**
 * Lightweight pdf document descriptor holding data necessary to fill the selection table and request a task execution.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PdfDocumentDescriptor {

    private ObservableAtomicReference<PdfDescriptorLoadingStatus> loadingStatus = new ObservableAtomicReference<>(
            PdfDescriptorLoadingStatus.INITIAL);
    private AtomicInteger references = new AtomicInteger(1);
    private ObservableAtomicReference<Integer> pages = new ObservableAtomicReference<>(0);
    private String password;
    private File file;
    private PdfVersion version;
    private Map<String, String> metadata = new HashMap<>();
    private SortedSet<Integer> validBookmarksLevels = Collections.emptySortedSet();

    private PdfDocumentDescriptor(File file, String password) {
        requireNotNullArg(file, "Input file is mandatory");
        this.file = file;
        this.password = password;
    }

    public String getFileName() {
        return file.getName();
    }

    public PdfFileSource toPdfFileSource() {
        try {
            return PdfFileSource.newInstanceWithPassword(file, password);
        } catch (IllegalArgumentException e) {
            throw new ConversionException(
                    DefaultI18nContext.getInstance().i18n("File \"{0}\" does not exist or is invalid", file.getName()),
                    e);
        }
    }

    /**
     * @param key
     * @return the information dictionary value for the key or an empty string
     */
    public String getInformation(String key) {
        return StringUtils.defaultString(metadata.get(key));
    }

    public void setInformationDictionary(Map<String, String> info) {
        metadata.clear();
        metadata.putAll(info);
    }

    public void putInformation(String key, String value) {
        metadata.put(key, value);
    }

    public void pages(int newValue) {
        this.pages.set(newValue);
    }

    public ObservableAtomicReference<PdfDescriptorLoadingStatus> loadingStatus() {
        return loadingStatus;
    }

    public ObservableAtomicReference<Integer> pages() {
        return pages;
    }

    /**
     * Sets the status to the given status destination
     * 
     * @param destination
     */
    public void moveStatusTo(PdfDescriptorLoadingStatus destination) {
        loadingStatus.set(loadingStatus.getValue().moveTo(destination));
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public boolean hasPassword() {
        return isNotBlank(password);
    }

    public String getVersionString() {
        return ofNullable(version).map(PdfVersion::getVersionString).orElse("");
    }

    public PdfVersion getVersion() {
        return version;
    }

    public void setVersion(PdfVersion version) {
        this.version = version;
    }

    public File getFile() {
        return file;
    }

    public void setValidBookmarksLevels(Set<Integer> levels) {
        this.validBookmarksLevels = new TreeSet<>();
        if (nonNull(levels)) {
            levels.stream().forEach(this.validBookmarksLevels::add);
        }
    }

    public SortedSet<Integer> getValidBookmarksLevels() {
        return validBookmarksLevels;
    }

    /**
     * @return true if this descriptor has references, this can be false if the user deletes it from the UI and it tells to any service performing or about to perform some action
     *         on the descriptor that it should be ignored since not relevant anymore.
     */
    public boolean hasReferences() {
        return references.get() > 0;
    }

    /**
     * @return true if the descriptor has become invalid because of the release
     */
    public boolean release() {
        return this.references.decrementAndGet() <= 0;
    }

    public void releaseAll() {
        this.references.set(0);
    }

    /**
     * Increment the number of reference
     */
    public PdfDocumentDescriptor retain() {
        this.references.incrementAndGet();
        return this;
    }

    public static PdfDocumentDescriptor newDescriptor(File file, String password) {
        return new PdfDocumentDescriptor(file, password);
    }

    public static PdfDocumentDescriptor newDescriptorNoPassword(File file) {
        return new PdfDocumentDescriptor(file, null);
    }

}
