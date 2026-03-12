/*
 * This file is part of the PDF Split And Merge source code
 * Created on 06/02/23
 * Copyright 2023 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.gui.components.sidebar;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.scene.Node;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.DataFormat;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.VBox;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.model.lifecycle.CleanupRequest;
import org.pdfsam.model.lifecycle.ShutdownEvent;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.persistence.PreferencesRepository;

import java.util.Comparator;
import java.util.Map;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

/**
 * @author Andrea Vacondio
 */
public class ToolsButtons extends VBox {

    private static final DataFormat DND_BUTTONS_MIME_TYPE = new DataFormat("application/x-java-tools-button");
    private static final String TOOL_ORDER = "toolOrder";
    private final PreferencesRepository repo;
    private boolean orderChanged;
    private final Comparator<ToolSelectableSidebarButton> toolsComparator = Comparator.comparing(
            ToolSelectableSidebarButton::category).thenComparing(ToolSelectableSidebarButton::getText);
    private final Comparator<ToolSelectableSidebarButton> toolsComparatorWithOrder = Comparator.comparing(
            ToolSelectableSidebarButton::order).thenComparing(toolsComparator);

    @Inject
    public ToolsButtons(@Named("toolsOrderRepository") PreferencesRepository repo) {
        this(repo, app().runtimeState().tools());
    }

    public ToolsButtons(PreferencesRepository repo, Map<String, Tool> tools) {
        getStyleClass().addAll("tools-buttons");
        this.repo = repo;
        tools.values().stream().map(t -> ToolSelectableSidebarButton.of(t, repo.getInt(TOOL_ORDER + "_" + t.id(), -1)))
                .sorted(toolsComparatorWithOrder).map(this::makeDraggable).forEach(this.getChildren()::add);
        eventStudio().addAnnotatedListeners(this);
    }

    private Node makeDraggable(Node node) {
        node.setOnDragDetected(e -> {
            Dragboard db = node.startDragAndDrop(TransferMode.MOVE);
            db.setDragView(node.snapshot(null, null));
            var cc = new ClipboardContent();
            cc.put(DND_BUTTONS_MIME_TYPE, this.getChildren().indexOf(node));
            db.setContent(cc);
            e.consume();
        });

        node.setOnDragOver(e -> {
            if (e.getDragboard().getContent(DND_BUTTONS_MIME_TYPE) instanceof Integer index) {
                if (index != this.getChildren().indexOf(node)) {
                    e.acceptTransferModes(TransferMode.MOVE);
                    e.consume();
                }
            }
        });

        node.setOnDragEntered(e -> {
            if (e.getDragboard().getContent(DND_BUTTONS_MIME_TYPE) instanceof Integer index) {
                if (index != this.getChildren().indexOf(node)) {
                    node.getStyleClass().add("drag-over");
                }
            }
        });

        node.setOnDragExited(e -> {
            if (e.getDragboard().getContent(DND_BUTTONS_MIME_TYPE) instanceof Integer index) {
                if (index != this.getChildren().indexOf(node)) {
                    node.getStyleClass().remove("drag-over");
                }
            }
        });

        node.setOnDragDropped(e -> {
            if (e.getDragboard().getContent(DND_BUTTONS_MIME_TYPE) instanceof Integer index) {
                var myIndex = this.getChildren().indexOf(node);
                if (index != myIndex) {
                    node.getStyleClass().remove("drag-over");
                    var dragged = this.getChildren().remove(index.intValue());
                    this.getChildren().add(this.getChildren().indexOf(node), dragged);
                    this.orderChanged = true;
                    e.setDropCompleted(true);
                    e.consume();
                }
            }
        });
        return node;
    }

    @EventListener
    public void onShutdown(ShutdownEvent event) {
        if (orderChanged) {
            for (int i = 0; i < this.getChildren().size(); i++) {
                if (this.getChildren().get(i) instanceof ToolSelectableSidebarButton t) {
                    repo.saveInt(TOOL_ORDER + "_" + t.id(), i);
                }
            }
        }
    }

    @EventListener
    public void onCleanupRequest(CleanupRequest req) {
        repo.clean();
        var sortedChildren = this.getChildren().stream().filter(n -> n instanceof ToolSelectableSidebarButton)
                .map(n -> (ToolSelectableSidebarButton) n).sorted(toolsComparator).toList();
        this.getChildren().setAll(sortedChildren);
    }
}
