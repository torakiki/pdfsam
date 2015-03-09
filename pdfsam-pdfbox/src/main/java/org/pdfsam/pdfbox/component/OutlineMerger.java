/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 06/mar/2015
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
import static org.pdfsam.pdfbox.component.PDFBoxOutlineUtils.copyOutlineDictionary;
import static org.pdfsam.pdfbox.component.PDFBoxOutlineUtils.toPageDestination;

import java.util.Optional;

import org.apache.pdfbox.pdmodel.PDDestinationNameTreeNode;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDDocumentNameDictionary;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination.PDPageDestination;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDDocumentOutline;
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDOutlineItem;
import org.sejda.common.collection.NullSafeSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component that keeps track of the relevant pages of a document, distills a cloned version of the document outline based on the relevant pages selected and merges it to a given
 * existing {@link PDDocumentOutline}.
 * 
 * @author Andrea Vacondio
 *
 */
public class OutlineMerger {
    private static final Logger LOG = LoggerFactory.getLogger(OutlineMerger.class);

    private Optional<PDDocumentOutline> outline;
    private PDDestinationNameTreeNode destinations = null;
    private NullSafeSet<PDPage> relevantPages = new NullSafeSet<>();

    public OutlineMerger(PDDocument document) {
        requireNonNull(document, "Unable to retrieve bookmarks from a null document.");
        PDDocumentNameDictionary names = document.getDocumentCatalog().getNames();
        if (names != null) {
            this.destinations = names.getDests();
        }
        this.outline = Optional.ofNullable(document.getDocumentCatalog().getDocumentOutline());
    }

    /**
     * @param page
     * @return adds a page that is relevant for the current task and any outline item pointing at this page as destination should be kept when merging the outline.
     */
    public boolean addRelevantPage(PDPage page) {
        return relevantPages.add(page);
    }

    public void mergeRelevantOutlineTo(PDDocumentOutline dest) {
        requireNonNull(dest, "Unable to merge relevant outline items to a null outline.");
        outline.ifPresent(o -> {
            for (PDOutlineItem child : o.children()) {
                cloneNode(child).ifPresent(c -> dest.addLast(c));
            }
            LOG.debug("Merged relevant outline items");
        });
    }

    private Optional<PDOutlineItem> cloneNode(PDOutlineItem node) {
        if (node.hasChildren()) {
            final PDOutlineItem clone = new PDOutlineItem();
            for (PDOutlineItem current : node.children()) {
                cloneNode(current).ifPresent(clonedChild -> {
                    clone.addLast(clonedChild);
                });
            }
            if (clone.hasChildren()) {
                copyOutlineDictionary(node, clone);
                Optional<PDPageDestination> destination = toPageDestination(node, destinations);
                if (isNeeded(destination)) {
                    copyDestination(destination, clone);
                }
                return Optional.of(clone);
            }
            return Optional.empty();
        }
        return cloneLeafIfNeeded(node);

    }

    private void copyDestination(Optional<PDPageDestination> destination, PDOutlineItem to) {
        destination.ifPresent(d -> {
            to.setDestination(d);
        });
    }

    /**
     * @param origin
     * @return a clone of the origin leaf if its page destination falls in the range of the needed pages. Cloned item destination is offset by the given offset.
     */
    private Optional<PDOutlineItem> cloneLeafIfNeeded(PDOutlineItem origin) {
        Optional<PDPageDestination> destination = toPageDestination(origin, destinations);
        if (isNeeded(destination)) {
            PDOutlineItem retVal = new PDOutlineItem();
            copyOutlineDictionary(origin, retVal);
            copyDestination(destination, retVal);
            return Optional.of(retVal);
        }
        return Optional.empty();
    }

    private boolean isNeeded(Optional<PDPageDestination> destination) {
        if (destination.isPresent()) {
            PDPage page = destination.get().getPage();
            if (page != null) {
                return relevantPages.contains(destination.get().getPage());
            }
        }
        return false;
    }
}
