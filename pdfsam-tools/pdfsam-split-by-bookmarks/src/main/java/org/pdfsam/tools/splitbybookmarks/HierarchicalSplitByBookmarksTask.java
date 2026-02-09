/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/nov/2025
 * Copyright 2025 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.tools.splitbybookmarks;

import org.sejda.commons.util.IOUtils;
import org.sejda.core.notification.context.GlobalNotificationContext;
import org.sejda.impl.sambox.component.DefaultPdfSourceOpener;
import org.sejda.impl.sambox.component.PDDocumentHandler;
import org.sejda.model.exception.TaskException;
import org.sejda.model.input.PdfSource;
import org.sejda.model.notification.event.PercentageOfWorkDoneChangedEvent;
import org.sejda.model.notification.event.TaskExecutionCompletedEvent;
import org.sejda.model.notification.event.TaskExecutionFailedEvent;
import org.sejda.model.notification.event.TaskExecutionStartedEvent;
import org.sejda.model.parameter.SplitByOutlineLevelParameters;
import org.sejda.model.pdf.PdfVersion;
import org.sejda.model.task.BaseTask;
import org.sejda.model.task.NotifiableTaskMetadata;
import org.sejda.model.task.TaskExecutionContext;
import org.sejda.sambox.pdmodel.PDDocument;
import org.sejda.sambox.pdmodel.PDPage;
import org.sejda.sambox.pdmodel.interactive.documentnavigation.outline.PDDocumentOutline;
import org.sejda.sambox.pdmodel.interactive.documentnavigation.outline.PDOutlineItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Task that splits a PDF by bookmarks creating a hierarchical directory structure.
 * Top-level bookmarks become directories, and their children become individual PDF files.
 *
 * @author Andrea Vacondio
 */
public class HierarchicalSplitByBookmarksTask extends BaseTask<SplitByOutlineLevelParameters> {

    private static final Logger LOG = LoggerFactory.getLogger(HierarchicalSplitByBookmarksTask.class);

    private PDDocumentHandler documentHandler;
    private DefaultPdfSourceOpener sourceOpener = new DefaultPdfSourceOpener();
    private int totalSteps = 0;
    private Pattern titleMatchingPattern;

    @Override
    public void before(SplitByOutlineLevelParameters parameters, TaskExecutionContext executionContext)
            throws TaskException {
        super.before(parameters, executionContext);
        if (parameters.getMatchingTitleRegEx() != null && !parameters.getMatchingTitleRegEx().isEmpty()) {
            titleMatchingPattern = Pattern.compile(parameters.getMatchingTitleRegEx());
        }
    }

    @Override
    public void execute(SplitByOutlineLevelParameters parameters) throws TaskException {
        requireNotNullArg(parameters, "Parameters cannot be null");

        try {
            GlobalNotificationContext.getContext()
                    .notifyListeners(new TaskExecutionStartedEvent(NotifiableTaskMetadata.NULL));

            PdfSource<?> source = parameters.getSourceList().get(0);
            LOG.debug("Opening PDF source {}", source);
            documentHandler = source.open(sourceOpener);

            PDDocumentOutline outline = documentHandler.getUnderlyingPDDocument().getDocumentCatalog().getDocumentOutline();
            if (outline == null) {
                throw new TaskException("No bookmarks found in the PDF document");
            }

            File outputDirectory = parameters.getOutput().getDestination();

            // Get overlap configuration if this is a hierarchical split
            int overlapPages = 0;
            boolean autoDetectOverlap = false;
            if (parameters instanceof HierarchicalSplitByOutlineLevelParameters) {
                HierarchicalSplitByOutlineLevelParameters hierarchicalParams = (HierarchicalSplitByOutlineLevelParameters) parameters;
                overlapPages = hierarchicalParams.getOverlapPages();
                autoDetectOverlap = hierarchicalParams.isAutoDetectOverlap();
                LOG.debug("Using overlap of {} pages, auto-detect: {}", overlapPages, autoDetectOverlap);
            }

            Map<String, List<BookmarkSection>> parentSections = extractHierarchicalBookmarks(outline,
                    parameters.getLevelToSplitAt());

            totalSteps = parentSections.values().stream().mapToInt(List::size).sum();

            if (totalSteps == 0) {
                throw new TaskException(
                        "No bookmarks found at the specified level. Please check the bookmark level and regex pattern.");
            }

            int currentStep = 0;
            for (Map.Entry<String, List<BookmarkSection>> entry : parentSections.entrySet()) {
                String parentName = entry.getKey();
                List<BookmarkSection> sections = entry.getValue();

                // Create directory for parent bookmark
                File parentDir = new File(outputDirectory, sanitizeFilename(parentName));
                try {
                    Files.createDirectories(parentDir.toPath());
                    LOG.debug("Created directory: {}", parentDir);
                } catch (Exception e) {
                    throw new TaskException("Failed to create directory: " + parentDir, e);
                }

                // Split each child section into separate PDF
                for (int i = 0; i < sections.size(); i++) {
                    BookmarkSection section = sections.get(i);
                    currentStep++;

                    // Determine actual overlap for this section
                    int actualOverlap = overlapPages;
                    if (autoDetectOverlap && i < sections.size() - 1) {
                        BookmarkSection nextSection = sections.get(i + 1);

                        // Auto-detect overlap in two cases:
                        // 1. Next section starts on the same page (bookmarks share a page)
                        // 2. Next section starts on the immediately following page (continuous content flow)
                        //    This handles cases where content from the current section continues onto
                        //    the page where the next bookmark starts

                        if (nextSection.startsOnSamePageAsPrevious) {
                            // Case 1: Bookmarks on same page - definitely include that page
                            actualOverlap = Math.max(actualOverlap, 1);
                            LOG.debug("Auto-detected overlap: next section '{}' starts on same page {}, including it",
                                    nextSection.title, nextSection.startPage);
                        } else if (section.endPage == nextSection.startPage) {
                            // Case 2: Next section starts exactly where current section ends
                            // This means they're on consecutive pages with no gap, likely continuous content
                            actualOverlap = Math.max(actualOverlap, 1);
                            LOG.debug("Auto-detected overlap: next section '{}' starts immediately at page {}, including it for completeness",
                                    nextSection.title, nextSection.startPage);
                        }
                    }

                    splitSection(section, parentDir, parameters, currentStep, actualOverlap);
                }
            }

            LOG.debug("Hierarchical split completed. Created {} files in {} directories", currentStep,
                    parentSections.size());

            GlobalNotificationContext.getContext()
                    .notifyListeners(new TaskExecutionCompletedEvent(0, NotifiableTaskMetadata.NULL));
        } catch (TaskException e) {
            GlobalNotificationContext.getContext().notifyListeners(new TaskExecutionFailedEvent(e, NotifiableTaskMetadata.NULL));
            throw e;
        } catch (Exception e) {
            GlobalNotificationContext.getContext().notifyListeners(new TaskExecutionFailedEvent(e, NotifiableTaskMetadata.NULL));
            throw new TaskException("Hierarchical split failed", e);
        }
    }

    private Map<String, List<BookmarkSection>> extractHierarchicalBookmarks(PDDocumentOutline outline, int targetLevel)
            throws TaskException {
        Map<String, List<BookmarkSection>> result = new HashMap<>();
        String currentParent = null;
        Integer previousPageInParent = null;

        try {
            PDOutlineItem current = outline.getFirstChild();
            int level = 1; // First level of bookmarks

            while (current != null) {
                if (level == targetLevel - 1) {
                    // This is a parent bookmark
                    currentParent = current.getTitle();
                    result.put(currentParent, new ArrayList<>());
                    previousPageInParent = null; // Reset for new parent
                } else if (level == targetLevel && currentParent != null) {
                    // This is a child bookmark under current parent
                    String title = current.getTitle();
                    if (titleMatchingPattern == null || titleMatchingPattern.matcher(title).matches()) {
                        Integer startPage = getPageNumber(current);
                        Integer endPage = getNextPageNumber(current);

                        if (startPage != null) {
                            if (endPage == null) {
                                endPage = documentHandler.getUnderlyingPDDocument().getNumberOfPages();
                            }

                            // Check if this bookmark starts on the same page as the previous one
                            boolean startsOnSamePage = previousPageInParent != null && startPage.equals(previousPageInParent);

                            result.get(currentParent).add(new BookmarkSection(title, startPage, endPage, startsOnSamePage));
                            LOG.debug("Added bookmark: {} (pages {}-{}), starts on same page: {}", title, startPage, endPage, startsOnSamePage);

                            previousPageInParent = startPage;
                        }
                    }
                }

                // Process children recursively
                if (current.hasChildren()) {
                    previousPageInParent = processChildren(current.getFirstChild(), targetLevel, currentParent, result, level + 1, previousPageInParent);
                }

                current = current.getNextSibling();
            }
        } catch (Exception e) {
            throw new TaskException("Failed to extract bookmark hierarchy", e);
        }

        return result;
    }

    private Integer processChildren(PDOutlineItem item, int targetLevel, String currentParent,
            Map<String, List<BookmarkSection>> result, int currentLevel, Integer previousPage) throws Exception {
        PDOutlineItem current = item;
        String localParent = currentParent;
        Integer lastPage = previousPage;

        while (current != null) {
            if (currentLevel == targetLevel - 1) {
                localParent = current.getTitle();
                result.put(localParent, new ArrayList<>());
                lastPage = null; // Reset for new parent
            } else if (currentLevel == targetLevel && localParent != null) {
                String title = current.getTitle();
                if (titleMatchingPattern == null || titleMatchingPattern.matcher(title).matches()) {
                    Integer startPage = getPageNumber(current);
                    Integer endPage = getNextPageNumber(current);

                    if (startPage != null) {
                        if (endPage == null) {
                            endPage = documentHandler.getUnderlyingPDDocument().getNumberOfPages();
                        }

                        // Check if this bookmark starts on the same page as the previous one
                        boolean startsOnSamePage = lastPage != null && startPage.equals(lastPage);

                        result.get(localParent).add(new BookmarkSection(title, startPage, endPage, startsOnSamePage));
                        LOG.debug("Added bookmark: {} (pages {}-{}), starts on same page: {}", title, startPage, endPage, startsOnSamePage);

                        lastPage = startPage;
                    }
                }
            }

            if (current.hasChildren()) {
                lastPage = processChildren(current.getFirstChild(), targetLevel, localParent, result, currentLevel + 1, lastPage);
            }

            current = current.getNextSibling();
        }

        return lastPage;
    }

    private Integer getPageNumber(PDOutlineItem bookmark) {
        try {
            PDDocument doc = documentHandler.getUnderlyingPDDocument();
            PDPage page = bookmark.findDestinationPage(doc);
            if (page != null) {
                return doc.getPages().indexOf(page) + 1; // 1-based
            }
        } catch (Exception e) {
            LOG.warn("Could not determine page for bookmark: {}", bookmark.getTitle(), e);
        }
        return null;
    }

    private Integer getNextPageNumber(PDOutlineItem bookmark) {
        try {
            PDOutlineItem next = bookmark.getNextSibling();
            if (next != null) {
                return getPageNumber(next);
            }
        } catch (Exception e) {
            LOG.warn("Could not determine next page for bookmark: {}", bookmark.getTitle(), e);
        }
        return null;
    }

    private void splitSection(BookmarkSection section, File parentDir, SplitByOutlineLevelParameters parameters,
            int currentStep, int overlapPages) throws TaskException {
        try {
            PDDocumentHandler handler = new PDDocumentHandler();
            handler.setCreatorOnPDDocument();
            handler.setVersionOnPDDocument(
                    parameters.getVersion() != null ? parameters.getVersion() : PdfVersion.VERSION_1_6);
            handler.setCompress(parameters.isCompress());

            // Calculate the actual end page with overlap
            int totalPages = documentHandler.getUnderlyingPDDocument().getNumberOfPages();
            int actualEndPage = Math.min(section.endPage + overlapPages, totalPages);

            LOG.debug("Splitting section '{}' from page {} to {} (with {} pages overlap)",
                    section.title, section.startPage, actualEndPage, overlapPages);

            // Extract pages for this section (including overlap)
            for (int pageNum = section.startPage; pageNum < actualEndPage; pageNum++) {
                handler.addPage(documentHandler.getUnderlyingPDDocument().getPage(pageNum - 1)); // SAMBox uses 0-based indexing
            }

            String filename = sanitizeFilename(section.title) + ".pdf";
            File outputFile = new File(parentDir, filename);

            handler.savePDDocument(outputFile);
            handler.close();

            LOG.debug("Created file: {}", outputFile);

            // Report progress
            BigDecimal percentComplete = BigDecimal.valueOf((currentStep / (double) totalSteps) * 100);
            GlobalNotificationContext.getContext()
                    .notifyListeners(new PercentageOfWorkDoneChangedEvent(percentComplete, NotifiableTaskMetadata.NULL));
        } catch (Exception e) {
            throw new TaskException("Failed to split section: " + section.title, e);
        }
    }

    private String sanitizeFilename(String filename) {
        if (filename == null || filename.isEmpty()) {
            return "unnamed";
        }
        // Replace invalid filename characters with underscores
        String sanitized = filename.replaceAll("[\\\\/:*?\"<>|]", "_");
        // Remove leading/trailing spaces and dots
        sanitized = sanitized.trim().replaceAll("^\\.+", "");
        // Limit length to 255 characters (common filesystem limit)
        if (sanitized.length() > 255) {
            sanitized = sanitized.substring(0, 255);
        }
        return sanitized.isEmpty() ? "unnamed" : sanitized;
    }

    @Override
    public void after() {
        IOUtils.closeQuietly(documentHandler);
    }

    private static class BookmarkSection {
        final String title;
        final int startPage;
        final int endPage;
        final boolean startsOnSamePageAsPrevious;

        BookmarkSection(String title, int startPage, int endPage, boolean startsOnSamePageAsPrevious) {
            this.title = title;
            this.startPage = startPage;
            this.endPage = endPage;
            this.startsOnSamePageAsPrevious = startsOnSamePageAsPrevious;
        }
    }
}
